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
  Alcotest.(check (list string)) "keyed leaves in order" [ "keys"; "clock" ] keys

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

let () =
  Alcotest.run "theaterminal"
    [
      ("event", [ Alcotest.test_case "parse" `Quick parse_cases ]);
      ("cmd", [ Alcotest.test_case "run" `Quick cmd_run ]);
      ("sub", [ Alcotest.test_case "leaves" `Quick sub_leaves ]);
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
        ] );
    ]
