module Make (R : Io_readiness.S) = struct
  type state =
    { run_queue : (unit -> unit) Queue.t
    ; io_readiness : R.t
    ; fd_waiters :
        ( Unix.file_descr
          , Actor.packed * (unit, unit) Stdlib.Effect.Deep.continuation )
          Hashtbl.t
    ; mutable timers :
        (float * Actor.packed * (unit, unit) Stdlib.Effect.Deep.continuation)
          list
    }

  let enqueue state f = Queue.push f state.run_queue

  let resume
    : type a.
      state
      -> Actor.packed
      -> (a, unit) Stdlib.Effect.Deep.continuation
      -> a
      -> unit
    =
    fun state (Actor.Packed c) k v ->
    enqueue state (fun () ->
      if c.Actor.alive then Stdlib.Effect.Deep.continue k v)
  ;;

  let cancel_waits state (Actor.Packed actor) =
    let waited_fd =
      Hashtbl.fold
        (fun fd (Actor.Packed c, _) acc ->
           if Actor.equal c actor then Some fd else acc)
        state.fd_waiters
        None
    in
    (match waited_fd with
     | Some fd ->
       Hashtbl.remove state.fd_waiters fd;
       R.unwatch_readable_fd state.io_readiness fd
     | None -> ());
    state.timers
    <- List.filter
         (fun (_, Actor.Packed c, _) -> not (Actor.equal c actor))
         state.timers
  ;;

  let await_readable state owner fd k =
    Hashtbl.replace state.fd_waiters fd (owner, k);
    R.watch_readable_fd state.io_readiness fd
  ;;

  let sleep state owner seconds k =
    let deadline = Unix.gettimeofday () +. seconds in
    state.timers <- (deadline, owner, k) :: state.timers
  ;;

  let finish actor =
    actor.Actor.alive <- false;
    actor.Actor.waiting <- None;
    Mailbox.clear actor.Actor.mailbox
  ;;

  let deliver : type a. state -> a Actor.t -> a -> unit =
    fun state actor msg ->
    if actor.Actor.alive
    then (
      Mailbox.push actor.Actor.mailbox msg;
      match actor.Actor.waiting with
      | None -> ()
      | Some (filter, k) ->
        (match Mailbox.take actor.Actor.mailbox filter with
         | exception exn ->
           actor.Actor.waiting <- None;
           enqueue state (fun () ->
             if actor.Actor.alive then Stdlib.Effect.Deep.discontinue k exn)
         | None -> ()
         | Some m ->
           actor.Actor.waiting <- None;
           enqueue state (fun () ->
             if actor.Actor.alive then Stdlib.Effect.Deep.continue k m)))
  ;;

  let rec handler
    : type a. state -> a Actor.t -> (unit, unit) Stdlib.Effect.Deep.handler
    =
    fun state actor ->
    { Stdlib.Effect.Deep.retc = (fun () -> finish actor)
    ; exnc =
        (fun e ->
          finish actor;
          raise e)
    ; effc =
        (fun (type b) (eff : b Stdlib.Effect.t) ->
          match eff with
          | Actor.Spawn action ->
            Some
              (fun (k : (b, unit) Stdlib.Effect.Deep.continuation) ->
                let c = Actor.create () in
                enqueue state (fun () ->
                  if c.Actor.alive
                  then
                    Stdlib.Effect.Deep.match_with action c (handler state c));
                Stdlib.Effect.Deep.continue k c)
          | Actor.Send (target, msg) ->
            Some
              (fun k ->
                deliver state target msg;
                Stdlib.Effect.Deep.continue k ())
          | Actor.Receive (self, filter) ->
            Some
              (fun k ->
                if not (Actor.equal self actor)
                then
                  Stdlib.Effect.Deep.discontinue
                    k
                    (Invalid_argument
                       "Troupe.receive: self belongs to another actor")
                else (
                  match Mailbox.take self.Actor.mailbox filter with
                  | exception exn -> Stdlib.Effect.Deep.discontinue k exn
                  | Some m -> Stdlib.Effect.Deep.continue k m
                  | None ->
                    if actor.Actor.alive
                    then self.Actor.waiting <- Some (filter, k)))
          | Actor.Await_readable fd ->
            Some
              (fun k ->
                if actor.Actor.alive
                then await_readable state (Actor.Packed actor) fd k)
          | Actor.Sleep seconds ->
            Some
              (fun k ->
                if actor.Actor.alive
                then sleep state (Actor.Packed actor) seconds k)
          | Actor.Stop target ->
            Some
              (fun k ->
                finish target;
                cancel_waits state (Actor.Packed target);
                Stdlib.Effect.Deep.continue k ())
          | _ -> None)
    }
  ;;

  let run_action state action cell =
    Stdlib.Effect.Deep.match_with action cell (handler state cell)
  ;;

  let earliest state () =
    match state.timers with
    | [] -> None
    | (d0, _, _) :: rest ->
      Some (List.fold_left (fun a (d, _, _) -> Float.min a d) d0 rest)
  ;;

  let fire_expired state () =
    let now = Unix.gettimeofday () in
    let expired, pending =
      List.partition (fun (d, _, _) -> d <= now) state.timers
    in
    state.timers <- pending;
    List.iter (fun (_, owner, k) -> resume state owner k ()) expired
  ;;

  let wait state timeout =
    R.wait state.io_readiness ~timeout (fun fd ->
      match Hashtbl.find_opt state.fd_waiters fd with
      | None -> ()
      | Some (owner, k) ->
        Hashtbl.remove state.fd_waiters fd;
        R.unwatch_readable_fd state.io_readiness fd;
        resume state owner k ())
  ;;

  let rec loop state () =
    fire_expired state ();
    if
      (not (Queue.is_empty state.run_queue))
      && Hashtbl.length state.fd_waiters > 0
    then wait state (`Int 0);
    match Queue.take_opt state.run_queue with
    | Some thunk ->
      thunk ();
      loop state ()
    | None ->
      if Hashtbl.length state.fd_waiters = 0 && state.timers = []
      then ()
      else (
        let timeout =
          match earliest state () with
          | None -> `Infinite
          | Some d -> `Float (Float.max 0.0 (d -. Unix.gettimeofday ()))
        in
        wait state timeout;
        loop state ())
  ;;

  let run main =
    let state =
      { run_queue = Queue.create ()
      ; io_readiness = R.create ()
      ; fd_waiters = Hashtbl.create 8
      ; timers = []
      }
    in
    let root : unit Actor.t = Actor.create () in
    enqueue state (fun () -> run_action state (fun _ -> main ()) root);
    loop state ()
  ;;
end
