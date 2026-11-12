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

let () =
  let integration =
    match Sys.getenv_opt "CARAVAN_STOCKFISH" with
    | None -> []
    | Some executable ->
      [ Alcotest.test_case "engine exchange" `Quick (stockfish executable) ]
  in
  Alcotest.run
    "modules"
    ([ ( "core"
       , [ Alcotest.test_case "initial state" `Quick state
         ; Alcotest.test_case "UCI" `Quick uci
         ] )
     ; "stockfish", integration
     ]
     @ Test_logic.suite)
;;
