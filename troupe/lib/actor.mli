type 'msg t =
  { id : int
  ; mailbox : 'msg Mailbox.t
  ; mutable waiting :
      (('msg -> bool) * ('msg, unit) Stdlib.Effect.Deep.continuation) option
  ; mutable alive : bool
  }

type 'msg behaviour = 'msg t -> unit
type packed = Packed : 'msg t -> packed

type _ Stdlib.Effect.t +=
  | Spawn : 'msg behaviour -> 'msg t Stdlib.Effect.t
  | Send : 'msg t * 'msg -> unit Stdlib.Effect.t
  | Receive : 'msg t * ('msg -> bool) -> 'msg Stdlib.Effect.t
  | Await_readable : Unix.file_descr -> unit Stdlib.Effect.t
  | Sleep : float -> unit Stdlib.Effect.t
  | Stop : 'msg t -> unit Stdlib.Effect.t

val create : unit -> 'msg t

val equal : 'a t -> 'b t -> bool
val pp : Format.formatter -> 'a t -> unit
