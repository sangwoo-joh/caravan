open Caravan_ui

let unwrap = function
  | Ok value -> value
  | Error error -> Alcotest.fail error
;;

let state () =
  let state = State.initial () in
  Alcotest.(check bool) "white starts" true (State.turn state = Piece.White);
  let occupied = ref 0 in
  for square = 0 to 63 do
    if Option.is_some (State.piece_at state square) then incr occupied
  done;
  Alcotest.(check int) "32 pieces" 32 !occupied;
  Alcotest.(check bool) "empty centre" true (State.piece_at state 28 = None);
  Alcotest.(check int) "balanced material" 0 (Logic.material_balance state);
  Alcotest.(check int) "pawn value" 100 (Logic.material_value Piece.Pawn)
;;

let uci () =
  List.iter
    (fun text ->
       Alcotest.(check string)
         "move round trip"
         text
         (Uci.move_to_string (unwrap (Uci.move_of_string text))))
    [ "e2e4"; "e7e8q"; "e2e5" ];
  List.iter
    (fun text ->
       Alcotest.(check bool)
         "reject malformed move"
         true
         (Result.is_error (Uci.move_of_string text)))
    [ ""; "e2"; "z2e4"; "e9e4"; "e7e8k"; "e2e4\nquit" ];
  Alcotest.(check string) "a8" "a8" (Uci.square_name 0);
  Alcotest.(check string) "h1" "h1" (Uci.square_name 63);
  let moves =
    List.map (fun text -> unwrap (Uci.move_of_string text)) [ "e2e4"; "e7e5" ]
  in
  Alcotest.(check string)
    "position command"
    "position startpos moves e2e4 e7e5"
    (unwrap (Uci.command_to_string (Uci.Position moves)));
  Alcotest.(check bool)
    "invalid search limit"
    true
    (Result.is_error (Uci.command_to_string (Uci.Go_movetime 0)));
  Alcotest.(check bool)
    "no move"
    true
    (unwrap (Uci.response_of_string "bestmove (none)") = Uci.Bestmove None);
  Alcotest.(check bool)
    "malformed response"
    true
    (Result.is_error (Uci.response_of_string "bestmove invalid"));
  match unwrap (Uci.response_of_string "bestmove g1f3 ponder b8c6\r") with
  | Uci.Bestmove (Some move) ->
    Alcotest.(check string) "best move" "g1f3" (Uci.move_to_string move)
  | _ -> Alcotest.fail "Expected bestmove"
;;

let stockfish executable () =
  let engine = unwrap (Stockfish.start executable) in
  Fun.protect
    ~finally:(fun () -> ignore (Stockfish.close engine))
    (fun () ->
       unwrap (Stockfish.send engine Uci.Uci);
       let rec until expected =
         if unwrap (Stockfish.receive engine) <> expected then until expected
       in
       until Uci.Uciok;
       unwrap (Stockfish.send engine Uci.Isready);
       until Uci.Readyok;
       unwrap (Stockfish.send engine (Uci.Position []));
       unwrap (Stockfish.send engine (Uci.Go_movetime 10));
       let rec bestmove () =
         match unwrap (Stockfish.receive engine) with
         | Uci.Bestmove (Some _) -> ()
         | Uci.Bestmove None -> Alcotest.fail "Initial position has legal moves"
         | _ -> bestmove ()
       in
       bestmove ())
;;

let prepare_input () =
  let start = State.initial () in
  let state = unwrap (Uci.prepare_move start "  e2e4 \t") in
  Alcotest.(check string)
    "normalized position"
    "position fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 \
     moves e2e4"
    (Uci.position_command state);
  List.iter
    (fun input ->
       Alcotest.(check bool)
         "invalid input"
         true
         (Result.is_error (Uci.prepare_move start input)))
    [ "e2e5"; "e7e5"; "e2e4\nquit"; "" ];
  let custom = Test_logic.position "7k/P7/8/8/8/8/8/7K w - - 0 1" in
  let promoted = unwrap (Uci.prepare_move custom "a7a8q") in
  Alcotest.(check string)
    "custom starting position"
    "position fen 7k/P7/8/8/8/8/8/7K w - - 0 1 moves a7a8q"
    (Uci.position_command promoted);
  Alcotest.(check string)
    "typed command"
    (Uci.position_command promoted)
    (unwrap (Uci.command_to_string (Uci.Position_state promoted)))
;;

let with_fake reply action =
  let path = Filename.temp_file "caravan-engine-" ".sh" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
       let output = open_out path in
       output_string
         output
         ("#!/bin/sh\nwhile IFS= read -r line; do\ncase \"$line\" in\n"
          ^ "uci) printf 'id name test\\nuciok\\n';;\n"
          ^ "isready) printf 'readyok\\n';;\n"
          ^ "go*) "
          ^ reply
          ^ ";;\nquit) exit 0;;\nesac\ndone\n");
       close_out output;
       Unix.chmod path 0o700;
       action path)
;;

let engine_pipeline () =
  with_fake
    "printf 'info depth 1\\nbestmove e7e5 ponder g1f3\\n'"
    (fun executable ->
       let result =
         unwrap (Stockfish.play_input ~executable (State.initial ()) " e2e4 ")
       in
       Alcotest.(check (option string))
         "parsed bestmove"
         (Some "e7e5")
         (Option.map Uci.move_to_string result.bestmove);
       Alcotest.(check (list string))
         "both moves applied"
         [ "e2e4"; "e7e5" ]
         (List.map Uci.move_to_string (State.moves result.state));
       Alcotest.(check (list string))
         "search info"
         [ "info depth 1" ]
         result.info);
  List.iter
    (fun reply ->
       with_fake reply (fun executable ->
         Alcotest.(check bool)
           "engine failure rejected"
           true
           (Result.is_error
              (Stockfish.play_input ~executable (State.initial ()) "e2e4"))))
    [ "printf 'bestmove e7e4\\n'"
    ; "printf 'bestmove nonsense\\n'"
    ; "printf 'bestmove (none)\\n'"
    ; "exit 1"
    ; "printf 'info string CRITICAL ERROR: failed\\n'"
    ];
  let missing = "/nonexistent/caravan-stockfish" in
  Alcotest.(check bool)
    "missing engine"
    true
    (Result.is_error (Stockfish.start missing));
  Alcotest.(check bool)
    "invalid move checked before launch"
    true
    (Stockfish.play_input ~executable:missing (State.initial ()) "e2e5"
     |> function
     | Error error -> error = "The piece cannot move to that square."
     | Ok _ -> false);
  let before_mate =
    Test_logic.sequence (State.initial ()) [ "f2f3"; "e7e5"; "g2g4" ]
  in
  let result =
    unwrap (Stockfish.play_input ~executable:missing before_mate "d8h4")
  in
  Alcotest.(check bool) "mate needs no engine" true (result.bestmove = None);
  with_fake ":" (fun executable ->
    let engine = unwrap (Stockfish.start executable) in
    Fun.protect
      ~finally:(fun () -> ignore (Stockfish.close engine))
      (fun () ->
         Alcotest.(check bool)
           "read timeout"
           true
           (Result.is_error (Stockfish.receive ~timeout:0.01 engine))))
;;

let stockfish_pipeline executable () =
  let result =
    unwrap
      (Stockfish.play_input ~movetime:20 ~executable (State.initial ()) "e2e4")
  in
  Alcotest.(check int)
    "player and engine moves"
    2
    (List.length (State.moves result.state));
  Alcotest.(check bool)
    "player turn restored"
    true
    (State.turn result.state = Piece.White);
  Alcotest.(check bool)
    "bestmove received"
    true
    (Option.is_some result.bestmove);
  let custom = Test_logic.position "7k/P7/8/8/8/8/8/7K w - - 0 1" in
  let result =
    unwrap (Stockfish.play_input ~movetime:20 ~executable custom "a7a8q")
  in
  Alcotest.(check int)
    "custom position reply"
    2
    (List.length (State.moves result.state))
;;

let () =
  let integration =
    match Sys.getenv_opt "CARAVAN_STOCKFISH" with
    | None -> []
    | Some executable ->
      [ Alcotest.test_case "engine exchange" `Quick (stockfish executable)
      ; Alcotest.test_case
          "input pipeline"
          `Quick
          (stockfish_pipeline executable)
      ]
  in
  Alcotest.run
    "modules"
    ([ ( "core"
       , [ Alcotest.test_case "initial state" `Quick state
         ; Alcotest.test_case "UCI" `Quick uci
         ; Alcotest.test_case "prepare input" `Quick prepare_input
         ; Alcotest.test_case "engine pipeline" `Quick engine_pipeline
         ] )
     ; "stockfish", integration
     ]
     @ Test_logic.suite)
;;
