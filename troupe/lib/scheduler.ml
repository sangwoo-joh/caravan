module Make (R : Reactor.S) = struct
  type state =
    { run_queue : (unit -> unit) Queue.t
    ; reactor : R.t
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

  let cancel_waits state (Actor.Packed cell) =
    let waited_fd =
      Hashtbl.fold
        (fun fd (Actor.Packed c, _) acc ->
           if Actor.equal c cell then Some fd else acc)
        state.fd_waiters
        None
    in
    (match waited_fd with
     | Some fd ->
       Hashtbl.remove state.fd_waiters fd;
       R.remove_reader state.reactor fd
     | None -> ());
    state.timers
    <- List.filter
         (fun (_, Actor.Packed c, _) -> not (Actor.equal c cell))
         state.timers
  ;;

  let await_readable state owner fd k =
    Hashtbl.replace state.fd_waiters fd (owner, k);
    R.add_reader state.reactor fd
  ;;

  let sleep state owner seconds k =
    let deadline = Unix.gettimeofday () +. seconds in
    state.timers <- (deadline, owner, k) :: state.timers
  ;;

  let finish cell =
    cell.Actor.alive <- false;
    cell.Actor.waiting <- None;
    Mailbox.clear cell.Actor.mailbox
  ;;

  let deliver : type a. state -> a Actor.t -> a -> unit =
    fun state cell msg ->
    if cell.Actor.alive
    then (
      Mailbox.push cell.Actor.mailbox msg;
      match cell.Actor.waiting with
      | None -> ()
      | Some (filter, k) ->
        (match Mailbox.take cell.Actor.mailbox filter with
         | exception exn ->
           cell.Actor.waiting <- None;
           enqueue state (fun () ->
             if cell.Actor.alive then Stdlib.Effect.Deep.discontinue k exn)
         | None -> ()
         | Some m ->
           cell.Actor.waiting <- None;
           enqueue state (fun () ->
             if cell.Actor.alive then Stdlib.Effect.Deep.continue k m)))
  ;;

  let rec handler
    : type a. state -> a Actor.t -> (unit, unit) Stdlib.Effect.Deep.handler
    =
    fun state cell ->
    { Stdlib.Effect.Deep.retc = (fun () -> finish cell)
    ; exnc =
        (fun e ->
          finish cell;
          raise e)
    ; effc =
        (fun (type b) (eff : b Stdlib.Effect.t) ->
          match eff with
          | Actor.Spawn behaviour ->
            Some
              (fun (k : (b, unit) Stdlib.Effect.Deep.continuation) ->
                let c = Actor.create () in
                enqueue state (fun () ->
                  if c.Actor.alive
                  then
                    Stdlib.Effect.Deep.match_with behaviour c (handler state c));
                Stdlib.Effect.Deep.continue k c)
          | Actor.Send (target, msg) ->
            Some
              (fun k ->
                deliver state target msg;
                Stdlib.Effect.Deep.continue k ())
          | Actor.Receive (self, filter) ->
            Some
              (fun k ->
                if not (Actor.equal self cell)
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
                    if cell.Actor.alive
                    then self.Actor.waiting <- Some (filter, k)))
          | Actor.Await_readable fd ->
            Some
              (fun k ->
                if cell.Actor.alive
                then await_readable state (Actor.Packed cell) fd k)
          | Actor.Sleep seconds ->
            Some
              (fun k ->
                if cell.Actor.alive
                then sleep state (Actor.Packed cell) seconds k)
          | Actor.Stop target ->
            Some
              (fun k ->
                finish target;
                cancel_waits state (Actor.Packed target);
                Stdlib.Effect.Deep.continue k ())
          | _ -> None)
    }
  ;;

  let run_behaviour state behaviour cell =
    Stdlib.Effect.Deep.match_with behaviour cell (handler state cell)
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
    R.wait state.reactor ~timeout (fun fd ->
      match Hashtbl.find_opt state.fd_waiters fd with
      | None -> ()
      | Some (owner, k) ->
        Hashtbl.remove state.fd_waiters fd;
        R.remove_reader state.reactor fd;
        resume state owner k ())
  ;;

  let rec loop state () =
    fire_expired state ();
    if
      (not (Queue.is_empty state.run_queue))
      && Hashtbl.length state.fd_waiters > 0
    then wait state (Some 0.);
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
          | None -> None
          | Some d -> Some (Float.max 0.0 (d -. Unix.gettimeofday ()))
        in
        wait state timeout;
        loop state ())
  ;;

  let run main =
    let state =
      { run_queue = Queue.create ()
      ; reactor = R.create ()
      ; fd_waiters = Hashtbl.create 8
      ; timers = []
      }
    in
    let root : unit Actor.t = Actor.create () in
    enqueue state (fun () -> run_behaviour state (fun _ -> main ()) root);
    loop state ()
  ;;
end
