open Lwt.Infix

let src = Logs.Src.create "captp" ~doc:"CapTP implementation"
module Log = (val Logs.src_log src: Logs.LOG)

let failf fmt = Fmt.kstrf failwith fmt

module Rpc = Schema.Make(Capnp.BytesMessage)

type target =
  | Question of Uint32.t

type ('a, 'b) proxy_method_t =
  ('a Capnp.BytesMessage.StructStorage.pointer_w -> unit) ->
  'b Capnp.BytesMessage.StructStorage.pointer_r option Lwt.t

type ('a, 'b) method_impl_t =
  'a Capnp.BytesMessage.StructStorage.pointer_r option ->
  'b Capnp.BytesMessage.StructStorage.pointer_w -> unit Lwt.t

type generic_method_t =
  | Abstr :
      ('a Capnp.BytesMessage.StructStorage.pointer_r option ->
       'b Capnp.BytesMessage.StructStorage.pointer_w -> unit Lwt.t) -> generic_method_t

let generic x = Abstr x

type service = interface_id:Uint64.t -> method_id:int -> generic_method_t

type t = {
  endpoint : Endpoint.t;
  my_bootstrap : service option;
  calls : (Uint32.t, Rpc.Reader.Return.t Lwt.u) Hashtbl.t;
  tags : Logs.Tag.set;
}

type client = {
  client : t;
  target : target;
}

let bootstrap t =
  let qid = Uint32.of_int 0 in
  let obj = { client = t; target = Question qid } in
  begin
    let open Rpc.Builder in
    let b = Message.init_root () in
    let boot = Message.bootstrap_init b in
    Bootstrap.question_id_set boot qid;
    let return, waker = Lwt.wait () in
    Hashtbl.add t.calls qid waker;
    Log.info (fun f -> f ~tags:t.tags "Requesting bootstrap service (qid=%a)" Uint32.printer qid);
    Endpoint.send t.endpoint (Message.to_message b) >>= fun () ->
    Lwt.return return
  end >|= fun return ->
  Lwt.async
    (fun () ->
       let open Rpc.Reader in
       return >|= Return.get >>= function
       | Return.Results _r ->
         Log.info (fun f -> f ~tags:t.tags "Got reply to bootstrap message");
         (* TODO: check it worked *)
         Lwt.return_unit
       | _ -> failwith "Unexpected reply to bootstrap message"
    );
  obj

let call (type a b) target ~interface_id ~method_id : (a, b) proxy_method_t = fun params ->
  let t = target.client in
  begin
    let open Rpc.Builder in
    let b = Message.init_root () in
    let c = Message.call_init b in
    let qid = Uint32.of_int 1 in   (* XXX: pipelining *)
    Log.info (fun f -> f ~tags:t.tags "Making call (qid=%a)" Uint32.printer qid);
    Call.question_id_set c qid;
    let () = (* Set target *)
      let Question qid = target.target in
      let msg_target = Call.target_init c in
      let pa = MessageTarget.promised_answer_init msg_target in
      PromisedAnswer.question_id_set pa qid
    in
    Call.interface_id_set c interface_id;
    Call.method_id_set_exn c method_id;
    let p = Call.params_init c in
    let args : a pointer_t = Payload.content_get p in
    params args;
    let return, waker = Lwt.wait () in
    Hashtbl.add t.calls qid waker;
    Endpoint.send t.endpoint (Message.to_message b) >>= fun () ->
    return
  end >>= fun return ->
  let open Rpc.Reader in
  match Return.get return with
  | Return.Results results ->
    let content : b pointer_t = Payload.content_get results in
    Lwt.return content
  | _ -> failwith "Unexpected return type received"

let handle_return t return =
  let open Rpc.Reader in
  let qid = Return.answer_id_get return in
  Log.info (fun f -> f ~tags:t.tags "Got answer to question %a" Uint32.printer qid);
  match Hashtbl.find t.calls qid with
  | exception Not_found -> failf "Got answer to question %a we weren't waiting for!" Uint32.printer qid
  | u ->
    (* TODO: remove from table *)
    Lwt.wakeup u return;
    Lwt.return_unit

let listen t : [`Never_returns] Lwt.t =
  let rec loop () =
    Endpoint.recv t.endpoint >>= fun msg ->
    let open Rpc.Builder in
    let m = Message.init_root () in
    let ret = Message.return_init m in
    begin
      let open Rpc.Reader in
      let msg = Message.of_message msg in
      match Message.get msg with
      | Message.Call call ->
        Log.info (fun f -> f ~tags:t.tags "Got call");
        let p = Call.params_get call in
        Rpc.Builder.Return.answer_id_set ret @@ Call.question_id_get call;
        let res = Rpc.Builder.Return.results_init ret in
        begin match t.my_bootstrap with
        | None -> assert false
        | Some dispatcher ->
          let interface_id = Call.interface_id_get call in
          let method_id = Call.method_id_get call in
          let Abstr method_impl = dispatcher ~interface_id ~method_id in
          method_impl (Payload.content_get p) (Rpc.Builder.Payload.content_get res) >>= fun () ->
          Log.info (fun f -> f ~tags:t.tags "Sending reply");
          let open Rpc.Builder in
          Endpoint.send t.endpoint (Message.to_message m) >>= fun () ->
          loop ()
        end
      | Message.Bootstrap boot ->
        Log.info (fun f -> f ~tags:t.tags "Got bootstrap request");
        Rpc.Builder.Return.answer_id_set ret @@ Bootstrap.question_id_get boot;
        let res = Rpc.Builder.Return.results_init ret in
        Rpc.Builder.Payload.content_set_interface res (Some Uint32.zero);       (* Cap index 0 *)
        let caps = Rpc.Builder.Payload.cap_table_init res 1 in
        let cap0 = Capnp.Array.get caps 0 in
        Rpc.Builder.CapDescriptor.sender_hosted_set cap0 Uint32.zero;     (* Export 0 *)
        Log.info (fun f -> f ~tags:t.tags "Sending reply");
        let open Rpc.Builder in
        Endpoint.send t.endpoint (Message.to_message m) >>= fun () ->
        loop ()
      | Message.Return return -> handle_return t return >>= loop
      | _ -> failwith "Not a call!"
    end
  in
  loop ()

let of_endpoint ?offer ?(tags=Logs.Tag.empty) endpoint =
  let t = { endpoint; my_bootstrap = offer; calls = Hashtbl.create 15; tags } in
  Lwt.async (fun () -> listen t);
  t
