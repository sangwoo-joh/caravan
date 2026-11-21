(** Subscriptions: external sources of messages. The runtime re-evaluates an
    app's subscriptions after every update and starts or stops sources so the
    active set always matches the current model. *)

type 'msg t

type 'msg source =
  | Keys of (KeyEvent.key -> 'msg option)
  | Every of float * (unit -> 'msg)
  | Worker :
      ('request Troupe.Actor.t -> dispatch:('msg -> unit) -> unit) * (unit -> unit)
      -> 'msg source

val none : 'msg t

(** [keys f] turns each key press into [f key], dispatching when it is [Some]. *)
val keys : (KeyEvent.key -> 'msg option) -> 'msg t

(** [every seconds f] dispatches [f ()] every [seconds]. Its diffing key
    defaults to the interval; pass [~key] to distinguish timers that share one,
    or to keep a timer stable while its interval changes. *)
val every : ?key:string -> float -> (unit -> 'msg) -> 'msg t

(** [worker ~key ~cleanup run] is a subscription running [run self ~dispatch]
    in an actor. [dispatch] sends a message to the app. The actor can publish
    [self] and receive requests sent with {!Cmd.send}.

    [key] must be unique among workers. Keeping the same key preserves the
    original [run] and [cleanup], even after [run] returns. Removing the key
    cancels the actor and calls [cleanup] once. Returning or raising from [run]
    also calls [cleanup] once. App termination calls it even if [run] has not
    started. Changing the key starts a new worker.

    [run] must use cooperative waits such as [Troupe.await_readable],
    [Troupe.receive], and [Troupe.sleep]. Blocking system calls block the app.
    [dispatch] is only valid within the app's Troupe scheduler. Child actors
    are not owned by this subscription.

    [cleanup] must not raise, block, or perform Troupe effects. It must handle
    partially acquired resources. Cancellation does not unwind [run], so
    resources must be released by [cleanup], rather than only by [Fun.protect]
    inside [run]. Unhandled worker exceptions terminate the app. *)
val worker
  :  key:string
  -> cleanup:(unit -> unit)
  -> ('request Troupe.Actor.t -> dispatch:('msg -> unit) -> unit)
  -> 'msg t

(** [batch subs] combines the subscriptions in [subs]. Each key press is
    delivered to every keyboard handler in list order. *)
val batch : 'msg t list -> 'msg t

(** Flatten to keyed leaf sources. Keys are stable across evaluations, so the
    runtime can diff two subscription sets and leave unchanged sources running.
    Used by the runtime. *)
val leaves : 'msg t -> (string * 'msg source) list
