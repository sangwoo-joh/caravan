(** A minimal actor model built on OCaml 5 effects.

    Each actor runs as its own fiber, processes messages from a private mailbox,
    and blocks in {!receive} until a matching message arrives. The scheduler in
    {!run} interprets the effects performed by [spawn], [send], and [receive].

    The API provides actor operations, {!Io_Readiness} I/O backends,
    {!Scheduler} execution, and a default {!run}. *)

module Actor : sig
  (** A handle to an actor accepting ['msg]. *)
  type 'msg t

  val equal : 'a t -> 'b t -> bool
  val pp : Format.formatter -> 'a t -> unit
end

(** The code an actor runs. It typically loops, pulling messages with
    {!receive}. *)
type 'msg behaviour = 'msg Actor.t -> unit

(** [spawn behaviour] spawns a new actor running [behaviour] and returns its
    address. *)
val spawn : 'msg behaviour -> 'msg Actor.t

(** [send addr msg] delivers [msg] to the actor at [addr]. Messages sent to a
    stopped or finished actor are discarded. *)
val send : 'msg Actor.t -> 'msg -> unit

(** [receive self ()] returns the oldest message in the mailbox, blocking the
    actor until one arrives. [receive self ~matching ()] instead returns the
    oldest message satisfying [matching], leaving the rest queued in order.
    Exceptions raised by [matching] are raised by [receive] without removing
    messages from the mailbox.

    @raise Invalid_argument if [self] belongs to another actor. *)
val receive : 'msg Actor.t -> ?matching:('msg -> bool) -> unit -> 'msg

(** [await_readable fd] blocks the actor until [fd] is readable. *)
val await_readable : Unix.file_descr -> unit

(** [sleep seconds] blocks the actor for at least [seconds]. *)
val sleep : float -> unit

(** [stop addr] cancels the actor at [addr]: it never runs again, and any
    descriptor or timer it was blocked on is released. Messages already queued
    for it are discarded. Stopping a finished actor is a no-op. Stopping yourself
    takes effect at your next suspension. *)
val stop : 'msg Actor.t -> unit

(** I/O readiness backends. Pass one to {!Scheduler.Make}. *)
module Io_Readiness = Io_readiness

(** Build a scheduler over a chosen {!Io_Readiness.S} backend. *)
module Scheduler = Scheduler

(** [run main] runs [main] as the initial fiber under a fresh scheduler backed
    by {!Io_Readiness.Select}, and returns once every actor is idle — blocked with no
    matching message, and with no outstanding descriptor or timer waits. [main]
    may {!spawn} actors and {!send} to them. *)
val run : (unit -> unit) -> unit
