open Lwt.Infix

let src = Logs.Src.create "endpoint" ~doc:"Send and receive Cap'n'Proto messages"
module Log = (val Logs.src_log src: Logs.LOG)

let compression = `None

let record_sent_messages = false

type t = {
  to_remote : Lwt_io.output Lwt_io.channel;
  from_remote : Lwt_io.input Lwt_io.channel;
  decoder : Capnp.Codecs.FramedStream.t;
}

let of_socket socket =
  let from_remote = Lwt_io.(of_unix_fd ~mode:input) socket in
  let to_remote = Lwt_io.(of_unix_fd ~mode:output) socket in
  let decoder = Capnp.Codecs.FramedStream.empty compression in
  { from_remote; to_remote; decoder }

let dump_msg =
  let next = ref 0 in
  fun data ->
    let name = Fmt.strf "/tmp/msg-%d.capnp" !next in
    Log.info (fun f -> f "Saved message as %S" name);
    incr next;
    let ch = open_out_bin name in
    output_string ch data;
    close_out ch

let send t msg =
  t.to_remote |> Lwt_io.atomic @@ fun to_remote ->
  let data = Capnp.Codecs.serialize ~compression msg in
  if record_sent_messages then dump_msg data;
  Lwt_io.write to_remote data

let rec recv t =
  match Capnp.Codecs.FramedStream.get_next_frame t.decoder with
  | Ok msg -> Lwt.return (Capnp.BytesMessage.Message.readonly msg)
  | Error Capnp.Codecs.FramingError.Unsupported -> failwith "Unsupported Cap'n'Proto frame received"
  | Error Capnp.Codecs.FramingError.Incomplete ->
    Log.debug (fun f -> f "Incomplete; waiting for more data...");
    Lwt_io.read ~count:4096 t.from_remote >>= fun data ->
    Log.debug (fun f -> f "Got %S" data);
    Capnp.Codecs.FramedStream.add_fragment t.decoder data;
    recv t
