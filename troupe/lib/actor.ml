type 'msg t =
  { id : int
  ; mailbox : 'msg Mailbox.t
  ; mutable waiting :
      (('msg -> bool) * ('msg, unit) Stdlib.Effect.Deep.continuation) option
  ; mutable alive : bool
  }

type 'msg action = 'msg t -> unit
type packed = Packed : 'msg t -> packed

type _ Stdlib.Effect.t +=
  | Spawn : 'msg action -> 'msg t Stdlib.Effect.t
  | Send : 'msg t * 'msg -> unit Stdlib.Effect.t
  | Receive : 'msg t * ('msg -> bool) -> 'msg Stdlib.Effect.t
  | Await_readable : Unix.file_descr -> unit Stdlib.Effect.t
  | Sleep : float -> unit Stdlib.Effect.t
  | Stop : 'msg t -> unit Stdlib.Effect.t

let next_id = Atomic.make 0

let create () =
  { id = Atomic.fetch_and_add next_id 1 + 1
  ; mailbox = Mailbox.create ()
  ; waiting = None
  ; alive = true
  }
;;

let equal a b = a.id = b.id
let pp fmt a = Format.fprintf fmt "<actor:%d>" a.id
