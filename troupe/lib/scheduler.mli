(** Actor scheduling over an I/O backend. *)

module Make (_ : Io_readiness.S) : sig
  (** [run main] runs [main] as the initial fiber under a fresh scheduler and
      returns once every actor is idle — blocked with no matching message, and
      with no outstanding descriptor or timer waits. [main] may [Troupe.spawn]
      actors and [Troupe.send] to them. *)
  val run : (unit -> unit) -> unit
end
