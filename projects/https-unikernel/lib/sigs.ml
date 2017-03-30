module type METHOD = sig
  module Params : sig
    type t

    val of_pointer : t Capnp.BytesMessage.StructStorage.pointer_r option -> t
  end

  module Results : sig
    type t

    val init_pointer : t Capnp.BytesMessage.StructStorage.pointer_w -> t
  end

  val handle : Params.t -> Results.t -> unit Lwt.t
end

module type REMOTE_METHOD = sig
  val method_id : int

  module Params : sig
    type t

    val init_pointer : t Capnp.BytesMessage.StructStorage.pointer_w -> t
  end

  module Results : sig
    type t

    val of_pointer : t Capnp.BytesMessage.StructStorage.pointer_r option -> t
  end
end
