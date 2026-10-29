open Theaterminal

let key = Alcotest.testable Event.pp Event.equal

let parse_cases () =
  let check input expected =
    Alcotest.(check (list key)) input expected (Event.parse input)
  in
  check "a" [ Event.Char 'a' ];
  check "ab" [ Event.Char 'a'; Event.Char 'b' ];
  check "\r" [ Event.Enter ];
  check "\n" [ Event.Enter ];
  check "\t" [ Event.Tab ];
  check "\x7f" [ Event.Backspace ];
  check "\x1b[A" [ Event.Up ];
  check "\x1b[B" [ Event.Down ];
  check "\x1b[C" [ Event.Right ];
  check "\x1b[D" [ Event.Left ];
  check "\x1b" [ Event.Escape ];
  check "\x1b[Aq" [ Event.Up; Event.Char 'q' ]

let parse_unknown_sequences () =
  List.iter
    (fun sequence ->
      Alcotest.(check (list key)) "whole sequence remains unknown"
        [ Event.Unknown sequence; Event.Char 'q' ] (Event.parse (sequence ^ "q")))
    [ "\x1b[3~"; "\x1b[15~"; "\x1b[1;5A"; "\x1b[?25h"; "\x1b[1 q" ];
  Alcotest.(check (list key)) "adjacent sequences"
    [ Event.Unknown "\x1b[3~"; Event.Up ] (Event.parse "\x1b[3~\x1b[A");
  Alcotest.(check (list key)) "incomplete sequence"
    [ Event.Unknown "\x1b[15" ] (Event.parse "\x1b[15");
  Alcotest.(check (list key)) "invalid byte remains available"
    [ Event.Unknown "\x1b["; Event.Enter ] (Event.parse "\x1b[\n")

let cmd_run () =
  let dispatched = ref [] in
  let quit = ref false in
  Cmd.run
    (Cmd.batch [ Cmd.msg 1; Cmd.msg 2; Cmd.quit ])
    ~dispatch:(fun m -> dispatched := m :: !dispatched)
    ~on_quit:(fun () -> quit := true);
  Alcotest.(check (list int)) "dispatched in order" [ 1; 2 ] (List.rev !dispatched);
  Alcotest.(check bool) "quit requested" true !quit

let sub_leaves () =
  let keys =
    List.map fst
      (Sub.leaves
         (Sub.batch
            [ Sub.keys (fun _ -> None); Sub.every ~key:"clock" 0.5 (fun () -> ()) ]))
  in
  Alcotest.(check (list string)) "keyed leaves in order" [ "keys"; "every:key:clock" ] keys

(* A counter app: [init] dispatches three increments, then the loop settles. *)
let loop_counts () =
  let app =
    {
      init = (0, Cmd.batch [ Cmd.msg (); Cmd.msg (); Cmd.msg () ]);
      update = (fun () n -> (n + 1, Cmd.none));
      view = string_of_int;
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let frames = ref [] in
  run_headless ~render:(fun s -> frames := s :: !frames) app;
  Alcotest.(check string) "last frame after 3 increments" "3" (List.hd !frames)

(* A quit command must stop the loop and let [run_headless] return. *)
let loop_quits () =
  let app =
    {
      init = (0, Cmd.msg ());
      update = (fun () n -> (n + 1, Cmd.quit));
      view = string_of_int;
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let frames = ref [] in
  run_headless ~render:(fun s -> frames := s :: !frames) app;
  Alcotest.(check string) "rendered once before quit" "1" (List.hd !frames)

(* A timer subscription is active only while the model is below 3. Once the
   model reaches 3 the subscription is dropped, the timer actor is stopped, and
   with no source left the loop settles at 3. The safety quit at 8 only fires if
   the timer was *not* stopped (a regression), preventing an infinite run. *)
let dynamic_sub_stops () =
  let app =
    {
      init = (0, Cmd.none);
      update =
        (fun () n -> if n >= 8 then (n, Cmd.quit) else (n + 1, Cmd.none));
      view = string_of_int;
      subscriptions =
        (fun n ->
          if n < 3 then Sub.every ~key:"t" 0.01 (fun () -> ()) else Sub.none);
    }
  in
  let frames = ref [] in
  run_headless ~render:(fun s -> frames := s :: !frames) app;
  Alcotest.(check string) "settled at 3 after timer stopped" "3" (List.hd !frames)

let with_timeout f =
  let previous =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "test timed out"))
  in
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm previous)
    (fun () ->
      ignore (Unix.alarm 2);
      f ())

let loop_quits_with_timer () =
  List.iter
    (fun immediately ->
      let ticks = ref 0 in
      with_timeout (fun () ->
          run_headless ~render:ignore
            {
              init = ((), if immediately then Cmd.quit else Cmd.none);
              update = (fun () () -> ((), Cmd.quit));
              view = (fun () -> "");
              subscriptions =
                (fun () -> Sub.every 0.001 (fun () -> incr ticks));
            });
      Alcotest.(check int) "timer stops at quit" (if immediately then 0 else 1)
        !ticks)
    [ true; false ]

let loop_quits_with_keys () =
  let saved = Unix.dup Unix.stdin in
  let reader, writer = Unix.pipe () in
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved Unix.stdin;
      List.iter Unix.close [ saved; reader; writer ])
    (fun () ->
      Unix.dup2 reader Unix.stdin;
      with_timeout (fun () ->
          run_headless ~render:ignore
            {
              init = ((), Cmd.none);
              update = (fun () () -> ((), Cmd.quit));
              view = (fun () -> "");
              subscriptions =
                (fun () ->
                  Sub.batch
                    [ Sub.keys (fun _ -> None); Sub.every 0.001 (fun () -> ()) ]);
            }))

let timer_refreshes_callback () =
  let values = ref [] in
  with_timeout (fun () ->
      run_headless ~render:ignore
        {
          init = (0, Cmd.none);
          update = (fun captured n -> values := captured :: !values; (n + 1, Cmd.none));
          view = string_of_int;
          subscriptions =
            (fun n ->
              if n < 2 then Sub.every ~key:"clock" 0.001 (fun () -> n)
              else Sub.none);
        });
  Alcotest.(check (list int)) "latest model" [ 0; 1 ] (List.rev !values)

let timer_refreshes_interval () =
  let times = ref [] in
  with_timeout (fun () ->
      run_headless ~render:ignore
        {
          init = (0, Cmd.none);
          update =
            (fun () n -> times := Unix.gettimeofday () :: !times; (n + 1, Cmd.none));
          view = string_of_int;
          subscriptions =
            (fun n ->
              if n < 2 then
                Sub.every ~key:"clock" (if n = 0 then 0.001 else 0.05) (fun () -> ())
              else Sub.none);
        });
  match !times with
  | [ second; first ] ->
    Alcotest.(check bool) "new interval is used" true (second -. first >= 0.05)
  | _ -> Alcotest.fail "expected two timer ticks"

let timer_keeps_deadline () =
  with_timeout (fun () ->
      run_headless ~render:ignore
        {
          init = ((), Cmd.none);
          update = (fun quit () -> ((), if quit then Cmd.quit else Cmd.none));
          view = (fun () -> "");
          subscriptions =
            (fun () -> Sub.batch
                [ Sub.every ~key:"fast" 0.001 (fun () -> false);
                  Sub.every ~key:"slow" 0.02 (fun () -> true) ]);
        })

let keys_refresh_callback () =
  let saved = Unix.dup Unix.stdin in
  let reader, writer = Unix.pipe () in
  let values = ref [] in
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved Unix.stdin;
      List.iter Unix.close [ saved; reader; writer ])
    (fun () ->
      Unix.dup2 reader Unix.stdin;
      ignore (Unix.write_substring writer "a" 0 1);
      with_timeout (fun () ->
          run_headless ~render:ignore
            {
              init = (0, Cmd.none);
              update = (fun captured n ->
                  values := captured :: !values;
                  if n = 0 then ignore (Unix.write_substring writer "b" 0 1);
                  (n + 1, if n = 1 then Cmd.quit else Cmd.none));
              view = string_of_int;
              subscriptions = (fun n -> Sub.keys (fun _ -> Some n));
            }));
  Alcotest.(check (list int)) "latest key handler" [ 0; 1 ] (List.rev !values)

external open_pty : unit -> Unix.file_descr * Unix.file_descr
  = "theaterminal_test_open_pty"

external set_terminal_size : Unix.file_descr -> int -> int -> unit
  = "theaterminal_test_set_size"

let with_terminal f =
  let master, slave = open_pty () in
  let saved_in = Unix.dup Unix.stdin and saved_out = Unix.dup Unix.stdout in
  flush stdout;
  Fun.protect
    ~finally:(fun () ->
      Terminal.release ();
      flush stdout;
      Unix.dup2 saved_in Unix.stdin;
      Unix.dup2 saved_out Unix.stdout;
      List.iter Unix.close [ master; slave; saved_in; saved_out ])
    (fun () ->
      Unix.dup2 slave Unix.stdin;
      Unix.dup2 slave Unix.stdout;
      f master)

let terminal_restores () =
  List.iter
    (fun fail ->
      with_terminal (fun _ ->
          let before = Unix.tcgetattr Unix.stdin in
          let app =
            {
              init = ((), Cmd.none);
              update = (fun () () -> ((), Cmd.none));
              view = (fun () -> if fail then failwith "view failed" else "frame");
              subscriptions = (fun () -> Sub.none);
            }
          in
          if fail then
            Alcotest.check_raises "view exception propagates" (Failure "view failed")
              (fun () -> run app)
          else run app;
          let after = Unix.tcgetattr Unix.stdin in
          Alcotest.(check bool) "terminal attributes restored" true (before = after);
          let changed = { before with c_echo = not before.c_echo } in
          Unix.tcsetattr Unix.stdin Unix.TCSANOW changed;
          Terminal.release ();
          Alcotest.(check bool) "release does not restore twice" true
            (changed = Unix.tcgetattr Unix.stdin)))
    [ false; true ]

let sub_distinct_keys () =
  let leaves =
    Sub.leaves
      (Sub.batch
         [ Sub.keys (fun _ -> None);
           Sub.every ~key:"keys" 1. (fun () -> ());
           Sub.every 1.0000001 (fun () -> ());
           Sub.every 1.0000002 (fun () -> ());
           Sub.every ~key:"interval:1.0000001" 1. (fun () -> ()) ])
  in
  Alcotest.(check int) "all identities are distinct" 5
    (List.length (List.sort_uniq String.compare (List.map fst leaves)))

let keys_batch () =
  let saved = Unix.dup Unix.stdin in
  let reader, writer = Unix.pipe () in
  let values = ref [] in
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved Unix.stdin;
      List.iter Unix.close [ saved; reader; writer ])
    (fun () ->
      Unix.dup2 reader Unix.stdin;
      ignore (Unix.write_substring writer "x" 0 1);
      with_timeout (fun () ->
          run_headless ~render:ignore
            {
              init = (0, Cmd.none);
              update = (fun value n ->
                  values := value :: !values;
                  (n + 1, if n = 2 then Cmd.quit else Cmd.none));
              view = string_of_int;
              subscriptions = (fun _ -> Sub.batch
                  [ Sub.keys (fun _ -> Some "first");
                    Sub.batch [ Sub.keys (fun _ -> Some "second") ];
                    Sub.every ~key:"keys" 0.01 (fun () -> "timer") ]);
            }));
  Alcotest.(check (list string)) "all handlers and timer run"
    [ "first"; "second"; "timer" ] (List.rev !values)

let terminal_size_preserves_input () =
  with_terminal (fun master ->
      set_terminal_size master 42 120;
      ignore (Unix.write_substring master "q" 0 1);
      with_timeout (fun () -> Terminal.setup ());
      Alcotest.(check (pair int int)) "actual terminal size" (42, 120)
        (Terminal.size ());
      let ready, _, _ = Unix.select [ Unix.stdin ] [] [] 0.1 in
      Alcotest.(check bool) "queued key is still readable" true (ready <> []);
      let buf = Bytes.create 1 in
      let n = Unix.read Unix.stdin buf 0 1 in
      Alcotest.(check string) "queued key is preserved" "q" (Bytes.sub_string buf 0 n))

let terminal_size_fallback () =
  with_terminal (fun master ->
      set_terminal_size master 0 0;
      Terminal.setup ();
      Alcotest.(check (pair int int)) "unknown terminal size" (24, 80)
        (Terminal.size ()))

let () =
  Alcotest.run "theaterminal"
    [
      ("event", [ Alcotest.test_case "parse" `Quick parse_cases;
                  Alcotest.test_case "unknown sequences" `Quick parse_unknown_sequences ]);
      ("terminal", [ Alcotest.test_case "restores" `Quick terminal_restores;
                     Alcotest.test_case "size preserves input" `Quick terminal_size_preserves_input;
                     Alcotest.test_case "size fallback" `Quick terminal_size_fallback ]);
      ("cmd", [ Alcotest.test_case "run" `Quick cmd_run ]);
      ("sub", [ Alcotest.test_case "leaves" `Quick sub_leaves;
                Alcotest.test_case "distinct keys" `Quick sub_distinct_keys ]);
      ( "loop",
        [
          Alcotest.test_case "counts" `Quick loop_counts;
          Alcotest.test_case "quits" `Quick loop_quits;
          Alcotest.test_case "quits with timer" `Quick loop_quits_with_timer;
          Alcotest.test_case "quits with keys" `Quick loop_quits_with_keys;
          Alcotest.test_case "dynamic sub stops" `Quick dynamic_sub_stops;
          Alcotest.test_case "timer callback refresh" `Quick timer_refreshes_callback;
          Alcotest.test_case "timer interval refresh" `Quick timer_refreshes_interval;
          Alcotest.test_case "timer keeps deadline" `Quick timer_keeps_deadline;
          Alcotest.test_case "keys callback refresh" `Quick keys_refresh_callback;
          Alcotest.test_case "keys batch" `Quick keys_batch;
        ] );
    ]
