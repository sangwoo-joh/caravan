open Caravan_ui

let unwrap = function
  | Ok value -> value
  | Error error -> Alcotest.fail error
;;

let move text = unwrap (Uci.move_of_string text)
let play state text = unwrap (Logic.play state (move text))
let sequence state moves = List.fold_left play state moves

let square text =
  ((8 - (Char.code text.[1] - Char.code '0')) * 8)
  + Char.code text.[0]
  - Char.code 'a'
;;

let at state text = State.piece_at state (square text)

let position fen =
  match String.split_on_char ' ' fen with
  | [ placement; turn; rights; ep; halfmove; fullmove ] ->
    let board = Array.make 64 None in
    let index = ref 0 in
    String.iter
      (fun c ->
         if c = '/'
         then ()
         else if c >= '1' && c <= '8'
         then index := !index + Char.code c - Char.code '0'
         else (
           let kind =
             match Char.lowercase_ascii c with
             | 'p' -> Piece.Pawn
             | 'n' -> Knight
             | 'b' -> Bishop
             | 'r' -> Rook
             | 'q' -> Queen
             | 'k' -> King
             | _ -> Alcotest.fail "Invalid fixture piece"
           in
           board.(!index)
           <- Some
                (Piece.make_piece
                   kind
                   (if c = Char.uppercase_ascii c then White else Black));
           incr index))
      placement;
    let castling_rights =
      List.filter_map
        (function
          | 'K' -> Some (Piece.White, State.Kingside)
          | 'Q' -> Some (White, Queenside)
          | 'k' -> Some (Black, Kingside)
          | 'q' -> Some (Black, Queenside)
          | _ -> None)
        (List.of_seq (String.to_seq rights))
    in
    unwrap
      (State.create
         ~castling_rights
         ?en_passant:(if ep = "-" then None else Some (square ep))
         ~halfmove_clock:(int_of_string halfmove)
         ~fullmove_number:(int_of_string fullmove)
         ~turn:(if turn = "w" then Piece.White else Black)
         board)
  | _ -> Alcotest.fail "Invalid fixture FEN"
;;

let valid state text expected =
  Alcotest.(check bool)
    text
    expected
    (Result.is_ok (Logic.validate_move state (move text)))
;;

let has_piece state text kind colour =
  Alcotest.(check bool)
    ("piece on " ^ text)
    true
    (match at state text with
     | Some piece -> piece.kind = kind && piece.colour = colour
     | None -> false)
;;

let empty state text =
  Alcotest.(check bool) ("empty " ^ text) true (at state text = None)
;;

let movement () =
  let state = State.initial () in
  List.iter
    (fun text -> valid state text true)
    [ "e2e4"; "e2e3"; "g1f3"; "b1a3" ];
  List.iter
    (fun text -> valid state text false)
    [ "e2e5"; "e7e5"; "a1a4"; "c1h6"; "e1e2"; "e3e4"; "e2f3"; "g1g3"; "e2e4q" ];
  let next = sequence state [ "e2e4"; "d7d5"; "e4d5" ] in
  has_piece state "e2" Pawn White;
  empty state "e4";
  empty next "e2";
  empty next "e4";
  has_piece next "d5" Pawn White;
  Alcotest.(check int) "capture resets clock" 0 (State.halfmove_clock next);
  Alcotest.(check int)
    "fullmove counts black moves"
    2
    (State.fullmove_number next);
  Alcotest.(check int) "material advantage" 100 (Logic.material_balance next);
  Alcotest.(check (list string))
    "history"
    [ "e2e4"; "d7d5"; "e4d5" ]
    (List.map Uci.move_to_string (State.moves next));
  Alcotest.(check bool)
    "previous snapshot"
    true
    (Option.is_some (State.previous next));
  let next = play next "g8f6" in
  Alcotest.(check int)
    "quiet move increments clock"
    1
    (State.halfmove_clock next)
;;

let setup () =
  let board = Array.make 64 None in
  board.(4) <- Some (Piece.make_piece King Black);
  board.(60) <- Some (Piece.make_piece King White);
  let state = unwrap (State.create ~turn:White board) in
  board.(60) <- None;
  has_piece state "e1" King White;
  Alcotest.(check bool)
    "missing king rejected"
    true
    (Result.is_error (State.create ~turn:White board));
  Alcotest.(check bool)
    "bad board size"
    true
    (Result.is_error (State.create ~turn:White [||]));
  List.iter
    (fun (source, target) ->
       Alcotest.(check bool)
         "invalid square"
         true
         (Result.is_error (State.move source target)))
    [ -1, 0; 0, 64; 0, 0 ];
  Alcotest.(check bool)
    "king promotion rejected"
    true
    (Result.is_error (State.move ~promotion:King 8 0))
;;

let king_safety () =
  let pinned = position "k3r3/8/8/8/8/8/4R3/4K3 w - - 0 1" in
  valid pinned "e2f2" false;
  valid pinned "e2e8" true;
  let checked = position "k3r3/8/8/8/8/8/8/4K3 w - - 0 1" in
  Alcotest.(check bool) "in check" true (Logic.in_check checked White);
  valid checked "e1e2" false;
  valid checked "e1d1" true;
  let pinned_attacker = position "4k3/4n3/8/5K2/8/8/8/4R3 w - - 0 1" in
  valid pinned_attacker "f5g6" false;
  let kings = position "8/8/8/8/8/4k3/8/4K3 w - - 0 1" in
  valid kings "e1e2" false;
  let king_capture = position "4k3/8/8/8/8/8/8/K3R3 w - - 0 1" in
  valid king_capture "e1e8" false
;;

let castling () =
  let start = position "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" in
  valid start "e1g1" true;
  valid start "e1c1" true;
  let short = play start "e1g1" in
  has_piece short "g1" King White;
  has_piece short "f1" Rook White;
  empty short "e1";
  empty short "h1";
  Alcotest.(check bool)
    "both white rights lost"
    false
    (State.can_castle short White Queenside);
  let long = play short "e8c8" in
  has_piece long "c8" King Black;
  has_piece long "d8" Rook Black;
  let returned = sequence start [ "a1a2"; "h8h7"; "a2a1"; "h7h8" ] in
  valid returned "e1c1" false;
  valid returned "e1g1" true;
  Alcotest.(check bool)
    "black rook right lost"
    false
    (State.can_castle returned Black Kingside);
  let captured = play start "a1a8" in
  Alcotest.(check bool)
    "captured rook loses right"
    false
    (State.can_castle captured Black Queenside);
  let moved_king = sequence start [ "e1f1"; "e8f8"; "f1e1"; "f8e8" ] in
  valid moved_king "e1g1" false;
  List.iter
    (fun fen -> valid (position fen) "e1g1" false)
    [ "k3r3/8/8/8/8/8/8/R3K2R w KQ - 0 1"
    ; "k4r2/8/8/8/8/8/8/R3K2R w KQ - 0 1"
    ; "k5r1/8/8/8/8/8/8/R3K2R w KQ - 0 1"
    ; "7k/8/8/8/8/8/8/R3KB1R w KQ - 0 1"
    ];
  valid (position "1r5k/8/8/8/8/8/8/R3K2R w KQ - 0 1") "e1c1" true
;;

let en_passant () =
  let start = sequence (State.initial ()) [ "e2e4"; "a7a6"; "e4e5"; "d7d5" ] in
  Alcotest.(check (option int))
    "en passant target"
    (Some (square "d6"))
    (State.en_passant start);
  valid start "e5d6" true;
  let next = play start "e5d6" in
  has_piece next "d6" Pawn White;
  empty next "d5";
  empty next "e5";
  has_piece start "d5" Pawn Black;
  Alcotest.(check (option int)) "target cleared" None (State.en_passant next);
  let expired = sequence start [ "g1f3"; "a6a5" ] in
  valid expired "e5d6" false;
  valid (position "8/8/8/r4pPK/8/8/8/k7 w - f6 0 1") "g5f6" false;
  let black = play (position "7k/8/8/8/3pP3/8/8/K7 b - e3 0 1") "d4e3" in
  has_piece black "e3" Pawn Black;
  empty black "e4"
;;

let promotion () =
  let state = position "1r5k/P7/8/8/8/8/8/7K w - - 0 1" in
  valid state "a7a8" false;
  List.iter
    (fun (suffix, kind) ->
       let promoted = play state ("a7a8" ^ suffix) in
       has_piece promoted "a8" kind White;
       let before = Option.get (at state "a7")
       and after = Option.get (at promoted "a8") in
       Alcotest.(check int) "promotion preserves identity" before.id after.id;
       Alcotest.(check string)
         "promotion changes figure"
         (Piece.figure kind White)
         after.figure)
    [ "q", Piece.Queen; "r", Rook; "b", Bishop; "n", Knight ];
  let captured = play state "a7b8q" in
  has_piece captured "b8" Queen White;
  let black = play (position "7k/8/8/8/8/8/p7/7K b - - 0 1") "a2a1n" in
  has_piece black "a1" Knight Black;
  let promotions =
    Logic.legal_moves state
    |> List.filter (fun (m : State.move) -> m.from_square = square "a7")
  in
  Alcotest.(check int) "all promotion choices" 8 (List.length promotions)
;;

let endings () =
  let mate = sequence (State.initial ()) [ "f2f3"; "e7e5"; "g2g4"; "d8h4" ] in
  Alcotest.(check bool)
    "fool's mate"
    true
    (Logic.status mate = Logic.Checkmate Piece.Black);
  Alcotest.(check bool) "no moves in mate" true (Logic.legal_moves mate = []);
  Alcotest.(check bool)
    "ended game rejects play"
    true
    (Result.is_error (Logic.play mate (move "e2e3")));
  let stale = position "7k/5K2/6Q1/8/8/8/8/8 b - - 0 1" in
  Alcotest.(check bool)
    "stalemate"
    true
    (Logic.status stale = Logic.Draw Stalemate);
  List.iter
    (fun fen ->
       Alcotest.(check bool)
         "insufficient material"
         true
         (Logic.status (position fen) = Logic.Draw Insufficient_material))
    [ "7k/8/8/8/8/8/8/K7 w - - 0 1"
    ; "7k/8/8/8/8/8/8/KB6 w - - 0 1"
    ; "7k/8/8/8/8/8/8/KN6 w - - 0 1"
    ; "5b1k/8/8/8/8/8/8/K1B5 w - - 0 1"
    ];
  Alcotest.(check bool)
    "two knights not automatically drawn"
    true
    (Logic.status (position "7k/8/8/8/8/8/8/KNN5 w - - 0 1") = Logic.Playing)
;;

let repetition () =
  let cycle = [ "g1f3"; "g8f6"; "f3g1"; "f6g8" ] in
  let twice = sequence (State.initial ()) cycle in
  Alcotest.(check bool)
    "two occurrences insufficient"
    true
    (Logic.claimable_draws twice = []);
  let before = sequence twice [ "g1f3"; "g8f6"; "f3g1" ] in
  Alcotest.(check bool)
    "claim with intended move"
    true
    (unwrap (Logic.claimable_draws_after before (move "f6g8"))
     = [ Logic.Threefold_repetition ]);
  let three = play before "f6g8" in
  Alcotest.(check bool)
    "threefold is claimable"
    true
    (Logic.claimable_draws three = [ Logic.Threefold_repetition ]);
  Alcotest.(check bool)
    "threefold does not end play"
    true
    (Logic.status three = Logic.Playing);
  let five = sequence (sequence three cycle) cycle in
  Alcotest.(check bool)
    "fivefold is automatic"
    true
    (Logic.status five = Logic.Draw Fivefold_repetition);
  let ep_without_capture = play (State.initial ()) "e2e4" in
  let cycle = [ "g8f6"; "g1f3"; "f6g8"; "f3g1" ] in
  let three = sequence (sequence ep_without_capture cycle) cycle in
  Alcotest.(check bool)
    "unusable en passant target ignored"
    true
    (Logic.claimable_draws three = [ Logic.Threefold_repetition ]);
  let start = position "4k1n1/8/8/3pP3/8/8/8/4K1N1 w - d6 0 1" in
  let cycle = [ "g1f3"; "g8f6"; "f3g1"; "f6g8" ] in
  Alcotest.(check bool)
    "usable en passant distinguishes positions"
    true
    (Logic.claimable_draws (sequence (sequence start cycle) cycle) = []);
  let start = position "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1" in
  let cycle = [ "a1a2"; "e8e7"; "a2a1"; "e7e8" ] in
  Alcotest.(check bool)
    "castling rights distinguish positions"
    true
    (Logic.claimable_draws (sequence (sequence start cycle) cycle) = [])
;;

let move_limits () =
  let before = position "7k/8/8/8/8/8/P7/R3K3 w - - 99 1" in
  Alcotest.(check bool) "99 halfmoves" true (Logic.claimable_draws before = []);
  Alcotest.(check bool)
    "intended 50th move"
    true
    (unwrap (Logic.claimable_draws_after before (move "a1b1"))
     = [ Logic.Fifty_moves ]);
  let fifty = play before "a1b1" in
  Alcotest.(check bool)
    "50 moves claimable"
    true
    (Logic.claimable_draws fifty = [ Logic.Fifty_moves ]);
  let reset = play before "a2a3" in
  Alcotest.(check bool)
    "pawn move resets claim"
    true
    (Logic.claimable_draws reset = []);
  let limit = position "7k/8/8/8/8/8/8/R3K3 w - - 149 1" in
  Alcotest.(check bool)
    "75 moves automatic"
    true
    (Logic.status (play limit "a1b1") = Logic.Draw Seventy_five_moves);
  let mating = position "7k/8/5KQ1/8/8/8/8/8 w - - 149 1" in
  Alcotest.(check bool)
    "mate takes precedence"
    true
    (Logic.status (play mating "g6g7") = Logic.Checkmate Piece.White)
;;

let rec perft state depth =
  if depth = 0
  then 1
  else
    List.fold_left
      (fun count move ->
         count + perft (unwrap (Logic.play state move)) (depth - 1))
      0
      (Logic.legal_moves state)
;;

let move_counts () =
  Alcotest.(check int) "initial depth 1" 20 (perft (State.initial ()) 1);
  Alcotest.(check int) "initial depth 2" 400 (perft (State.initial ()) 2);
  Alcotest.(check int) "initial depth 3" 8902 (perft (State.initial ()) 3);
  Alcotest.(check int)
    "castling and pins depth 2"
    2039
    (perft
       (position
          "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
       2);
  Alcotest.(check int)
    "rook endgame depth 3"
    2812
    (perft (position "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1") 3);
  Alcotest.(check int)
    "promotions and checks depth 2"
    264
    (perft
       (position
          "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
       2)
;;

let suite =
  [ ( "logic"
    , List.map
        (fun (name, test) -> Alcotest.test_case name `Quick test)
        [ "movement and snapshots", movement
        ; "state construction", setup
        ; "king safety", king_safety
        ; "castling", castling
        ; "en passant", en_passant
        ; "promotion", promotion
        ; "game endings", endings
        ; "repetition", repetition
        ; "move limits", move_limits
        ; "legal move counts", move_counts
        ] )
  ]
;;
