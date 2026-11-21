(** I/O readiness backends for the scheduler.

    The backend watches file descriptors and reports when they become readable, so
    the scheduler can block instead of busy-waiting while every actor is idle. *)

module type S = sig
  type t

  val create : unit -> t
  val watch_readable_fd : t -> Unix.file_descr -> unit
  val unwatch_readable_fd : t -> Unix.file_descr -> unit

  (** [wait t ~timeout f] blocks until a registered descriptor is readable or
      [timeout] seconds elapse ([None] blocks indefinitely), then calls [f] once
      per readable descriptor. *)
  val wait : t -> timeout:float option -> (Unix.file_descr -> unit) -> unit
end

(** A portable backend built on {!Unix.select}. Fine for a handful of
    descriptors; swap in {!Poll} (or an [epoll]/[kqueue] backend) behind {!S} if
    you ever watch many. *)
module Select : S

(** A [poll(2)]-based backend using the [iomux] library. Portable
    (Linux/macOS/BSD) and free of [select]'s descriptor-count limit. *)
module Poll : S
