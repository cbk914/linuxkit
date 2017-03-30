(** Provides the RPC layer on top of [Endpoint]. *)

type t

include Capnp.RPC.S with
  type ('a, 'b) proxy_method_t =
    ('a Capnp.BytesMessage.StructStorage.pointer_w -> unit) ->
    'b Capnp.BytesMessage.StructStorage.pointer_r option Lwt.t and
  type ('a, 'b) method_impl_t =
    'a Capnp.BytesMessage.StructStorage.pointer_r option ->
    'b Capnp.BytesMessage.StructStorage.pointer_w -> unit Lwt.t

type service = interface_id:Uint64.t -> method_id:int -> generic_method_t

val of_endpoint : ?offer:service -> ?tags:Logs.Tag.set -> Endpoint.t -> t
(** [of_endpoint ?offer endpoint] is fresh CapTP state for communicating with [endpoint].
    If [offer] is given, the peer can use the "Bootstrap" message to get access to it. *)

val bootstrap : t -> client Lwt.t
(** [bootstrap t] is the peer's public bootstrap object, if any. *)
