(** A FIFO mailbox supporting selective removal. *)

type 'a t

val create : unit -> 'a t

(** [clear t] removes all messages from [t]. *)
val clear : 'a t -> unit

(** [push t x] appends [x] to the back of [t]. *)
val push : 'a t -> 'a -> unit

(** [take t p] removes and returns the oldest element satisfying [p], leaving
    the remaining elements in their original order. Returns [None] if no element
    satisfies [p]. *)
val take : 'a t -> ('a -> bool) -> 'a option

val is_empty : 'a t -> bool
