(* Send and receive capnp messages over a socket. *)

type t

val of_socket : Unix.file_descr -> t

val send : t -> 'a Capnp.BytesMessage.Message.t -> unit Lwt.t
(** [send t msg] transmits [msg] atomically. *)

val recv : t -> Capnp.Message.ro Capnp.BytesMessage.Message.t Lwt.t
(* todo: report end-of-file *)
