module Event = Event
module Terminal = Terminal
module Cmd = Cmd
module Sub = Sub

type ('model, 'msg) app =
  { init : 'model * 'msg Cmd.t
  ; update : 'msg -> 'model -> 'model * 'msg Cmd.t
  ; view : 'model -> string
  ; subscriptions : 'model -> 'msg Sub.t
  }

type 'msg source =
  | Keys of (Event.key -> 'msg option) list
  | Every of float * (unit -> 'msg)
  | Worker :
      ('request Troupe.self -> dispatch:('msg -> unit) -> unit) * (unit -> unit)
      -> 'msg source

let sources subscriptions =
  let leaves = Sub.leaves subscriptions in
  let keys =
    List.filter_map
      (function
        | _, Sub.Keys f -> Some f
        | _, (Sub.Every _ | Sub.Worker _) -> None)
      leaves
  in
  let workers =
    List.filter_map
      (function
        | key, Sub.Every (seconds, f) -> Some (key, Every (seconds, f))
        | key, Sub.Worker (run, on_stop) -> Some (key, Worker (run, on_stop))
        | _, Sub.Keys _ -> None)
      leaves
  in
  match keys with
  | [] -> workers
  | _ -> ("keys", Keys keys) :: workers
;;

(* One actor per subscription source, each feeding [runtime]'s mailbox. *)
let spawn_source source ~runtime =
  match source with
  | Keys f ->
    let callback = ref f in
    let actor =
      Troupe.cast (fun _ ->
        let buf = Bytes.create 1024 in
        let rec loop () =
          Troupe.await_readable Unix.stdin;
          let n = Unix.read Unix.stdin buf 0 (Bytes.length buf) in
          if n > 0
          then (
            List.iter
              (fun k ->
                 List.iter
                   (fun f -> Option.iter (Troupe.send runtime) (f k))
                   !callback)
              (Event.parse (Bytes.sub_string buf 0 n));
            loop ())
        in
        loop ())
    in
    ( (fun () -> Troupe.stop actor)
    , ignore
    , (function
        | Keys f ->
          callback := f;
          true
        | Every _ | Worker _ -> false) )
  | Every (seconds, f) ->
    let callback = ref f in
    let actor =
      Troupe.cast (fun _ ->
        let rec loop () =
          Troupe.sleep seconds;
          Troupe.send runtime (!callback ());
          loop ()
        in
        loop ())
    in
    ( (fun () -> Troupe.stop actor)
    , ignore
    , (function
        | Every (interval, f) when interval = seconds ->
          callback := f;
          true
        | _ -> false) )
  | Worker (run, on_stop) ->
    let stopped = ref false in
    let cleanup () =
      if not !stopped
      then (
        stopped := true;
        on_stop ())
    in
    let actor =
      Troupe.cast (fun self ->
        Fun.protect ~finally:cleanup (fun () ->
          run self ~dispatch:(Troupe.send runtime)))
    in
    ( (fun () ->
        Troupe.stop actor;
        cleanup ())
    , cleanup
    , (function
        | Worker _ -> true
        | _ -> false) )
;;

let run_app ~render ~on_quit app =
  let init_model, init_cmd = app.init in
  let registry = Hashtbl.create 8 in
  Fun.protect
    ~finally:(fun () ->
      Hashtbl.iter (fun _ (_, cleanup, _) -> cleanup ()) registry)
    (fun () ->
       Troupe.run (fun () ->
         ignore
           (Troupe.cast (fun self ->
              let runtime = Troupe.address self in
              let dispatch m = Troupe.send runtime m in
              let stop = ref false in
              let request_quit () = stop := true in
              (* Diff the desired subscriptions against the running ones: stop the
                sources whose key disappeared, start the ones that appeared. *)
              let reconcile model =
                let wanted = sources (app.subscriptions model) in
                let keep key = List.mem_assoc key wanted in
                Hashtbl.iter
                  (fun key (cancel, _, _) -> if not (keep key) then cancel ())
                  registry;
                Hashtbl.filter_map_inplace
                  (fun key addr -> if keep key then Some addr else None)
                  registry;
                List.iter
                  (fun (key, source) ->
                     match Hashtbl.find_opt registry key with
                     | Some (_, _, refresh) when refresh source -> ()
                     | existing ->
                       Option.iter (fun (cancel, _, _) -> cancel ()) existing;
                       Hashtbl.replace
                         registry
                         key
                         (spawn_source source ~runtime))
                  wanted
              in
              let model = ref init_model in
              render (app.view !model);
              reconcile !model;
              Cmd.run init_cmd ~dispatch ~on_quit:request_quit;
              let rec loop () =
                if !stop
                then (
                  Hashtbl.iter (fun _ (cancel, _, _) -> cancel ()) registry;
                  Hashtbl.clear registry;
                  on_quit ())
                else (
                  let msg = Troupe.receive self () in
                  let model', cmd = app.update msg !model in
                  model := model';
                  render (app.view model');
                  Cmd.run cmd ~dispatch ~on_quit:request_quit;
                  if not !stop then reconcile model';
                  loop ())
              in
              loop ()))))
;;

let run app =
  Fun.protect ~finally:Terminal.release (fun () ->
    Terminal.setup ();
    run_app app ~render:Terminal.render ~on_quit:(fun () ->
      Terminal.release ();
      exit 0))
;;

let run_headless ~render app = run_app app ~render ~on_quit:ignore
