(** Commands: side effects an [update] asks the runtime to perform, optionally
    feeding new messages back into the loop. *)

type 'msg t

(** Do nothing. *)
val none : 'msg t

(** [msg m] dispatches [m] back into the loop as the next message. *)
val msg : 'msg -> 'msg t

(** Stop the program. *)
val quit : 'msg t

(** [send address request] is a command delivering [request] to [address]
    without waiting for a reply. The recipient can send results through a
    worker's [dispatch] callback. Requests to stopped actors are discarded.
    This command must be interpreted within a Troupe scheduler. *)
val send : 'request Troupe.Address.t -> 'request -> 'msg t

(** Run several commands, left to right. *)
val batch : 'msg t list -> 'msg t

(** Interpret a command, calling [dispatch] for each {!msg} and [on_quit] for
    {!quit}. Used by the runtime; applications build commands with the
    combinators above. *)
val run : 'msg t -> dispatch:('msg -> unit) -> on_quit:(unit -> unit) -> unit
