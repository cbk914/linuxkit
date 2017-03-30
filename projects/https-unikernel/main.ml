open Lwt.Infix
open Astring
open CapTP

module Reporter = Mirage_logs.Make(Pclock)
module Proto = Proto.MakeRPC(CapTP.Connection)(Capnp.BytesMessage)

let store_to_http, http_to_store = Unix.(socketpair PF_UNIX SOCK_STREAM 0)

let endpoint_tag = Logs.Tag.def "endpoint" Fmt.string

module Store = struct
  let src = Logs.Src.create "web.store" ~doc:"Datastore for web server"
  module Log = (val Logs.src_log src: Logs.LOG)

  (* The application logic *)
  let get = function
    | ["index.html"] -> Some "The index"
    | _ -> None

  (* The Cap'n'Proto service interface we expose. *)
  let service =
    Proto.Builder.Store.dispatch @@
      object
        method get params results =
          let module P = Proto.Reader.Store.Get_params in
          let module R = Proto.Builder.Store.Response in
          let params = P.of_pointer params in
          let path = P.path_get_list params in
          Log.info (fun f -> f "Handing request for %a" (Fmt.Dump.list String.dump) path);
          let results = R.init_pointer results in
          begin match get path with
            | Some data -> R.ok_set results data
            | None -> R.not_found_set results
          end;
          Lwt.return ()
      end

  let init () =
    let tags = Logs.Tag.add endpoint_tag "Store" Logs.Tag.empty in
    ignore (Connection.of_endpoint ~offer:service ~tags @@ Endpoint.of_socket store_to_http)
end

module HTTP_server = struct
  let src = Logs.Src.create "web.http" ~doc:"HTTP engine for web server"
  module Log = (val Logs.src_log src: Logs.LOG)

  module Server = Cohttp_lwt.Make_server(Cohttp_lwt_unix_io)

  type t = {
    store : Proto.Reader.Store.client;
  }

  let connect () =
    let endpoint = Endpoint.of_socket http_to_store in
    let tags = Logs.Tag.add endpoint_tag "Server" Logs.Tag.empty in
    Connection.bootstrap (Connection.of_endpoint ~tags endpoint) >>= fun store ->
    Lwt.return { store = new Proto.Reader.Store.client store }

  (* Make a Cap'n'Proto call to the store service *)
  let get t path =
    t.store#get (fun ptr ->
        let open Proto.Builder.Store in
        let params = Get_params.init_pointer ptr in
        ignore (Get_params.path_set_list params path)
      )
    >>= fun results ->
    let open Proto.Reader.Store in
    match Response.get (Response.of_pointer results) with
    | Response.NotFound -> Lwt.return None
    | Response.Ok data -> Lwt.return (Some data)
    | Response.Undefined _ -> failwith "Protocol error: bad msg type"

  (* Handle HTTP requests *)
  let callback t _conn req _body =
    let open Cohttp in
    let uri = Request.uri req in
    Log.info (fun f -> f "HTTP request for %a" Uri.pp_hum uri);
    match Request.meth req with
    | `GET ->
      let path = String.cuts ~empty:false ~sep:"/" (Uri.path uri) in
      let path =
        match path with
        | [] -> ["index.html"]
        | p -> p
      in
      begin get t path >>= function
      | Some body -> Server.respond_string ~status:`OK ~body ()
      | None -> Server.respond_not_found ~uri ()
      end
    | m ->
      let body = Fmt.strf "Bad method %S" (Code.string_of_method m) in
      Server.respond_error ~status:`Bad_request ~body ()

  let callback t = Server.callback (Server.make ~callback:(callback t) ())
end

module TLS = struct
  let mode = `TCP (`Port 8000)  (* TODO: TLS *)

  let listen http =
    Conduit_lwt_unix.(serve ~ctx:default_ctx) ~mode (fun flow ic oc ->
        HTTP_server.callback http flow ic oc
      )
end

let () =
  Lwt_main.run begin
    Pclock.connect () >|= Reporter.create >|= Reporter.set_reporter >>= fun () ->
    Logs.set_level (Some Logs.Info);
    Store.init ();
    HTTP_server.connect () >>= TLS.listen
  end
