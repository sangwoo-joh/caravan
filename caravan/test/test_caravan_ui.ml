open Theaterminal

let step model key = fst (Caravan_ui.update key model)
let steps keys = List.fold_left step Caravan_ui.initial keys
let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0
let shows model text =
  Alcotest.(check bool) text true (contains (Caravan_ui.view model) text)
let quit model key =
  let _, command = Caravan_ui.update key model in
  let quit = ref false in
  Cmd.run command ~dispatch:ignore ~on_quit:(fun () -> quit := true);
  !quit

let landing () =
  List.iter (shows Caravan_ui.initial) [ "Caravan"; "> Play"; "Load"; "Setting"; "Exit" ];
  List.iter (fun key ->
      Alcotest.(check string) "unsupported input is ignored"
        (Caravan_ui.view Caravan_ui.initial)
        (Caravan_ui.view (step Caravan_ui.initial key)))
    [ Event.Char 'q'; Event.Escape; Event.Tab; Event.Char 'e' ];
  shows (steps [ Event.Down; Event.Enter ]) "No session save yet";
  let exit = steps [ Event.Down; Event.Down; Event.Down ] in
  Alcotest.(check bool) "Exit confirms" true (quit exit Event.Enter);
  Alcotest.(check bool) "Ctrl+C quits" true (quit Caravan_ui.initial (Event.Unknown "\003"))

let settings () =
  let model = steps [ Event.Down; Event.Down; Event.Enter; Event.Down; Event.Char ' ' ] in
  shows model "(*) Hard";
  let model = List.fold_left step model
      [ Event.Down; Event.Enter; Event.Up; Event.Up; Event.Enter ] in
  shows model "Caravan / Play";
  shows model "Difficulty: Hard";
  Alcotest.(check bool) "Ctrl+C quits play" true (quit model (Event.Unknown "\003"))

let move_preview () =
  let model = steps [ Event.Enter; Event.Enter; Event.Up; Event.Up; Event.Char ' ' ] in
  shows model "Last preview: e2e4";
  shows model "Move preview only";
  let model = List.fold_left step model
      [ Event.Char 'e'; Event.Char '7'; Event.Char 'e'; Event.Char '8';
        Event.Char 'r'; Event.Backspace; Event.Char 'q'; Event.Enter ] in
  shows model "Last preview: e7e8q";
  let model = List.fold_left step model [ Event.Char 'e'; Event.Enter ] in
  shows model "Use a move such as e2e4";
  shows model "Last preview: e7e8q"

let save_load () =
  let model = steps
      [ Event.Enter; Event.Enter; Event.Up; Event.Up; Event.Enter;
        Event.Right; Event.Right; Event.Right; Event.Right; Event.Enter;
        Event.Down; Event.Enter; Event.Enter ] in
  shows model "Last preview: -";
  let model = List.fold_left step model
      [ Event.Right; Event.Right; Event.Right; Event.Right; Event.Down;
        Event.Enter; Event.Down; Event.Enter ] in
  shows model "Last preview: e2e4";
  shows model "Loaded the latest session save";
  let model = List.fold_left step model
      [ Event.Char 'd'; Event.Char '2'; Event.Char 'd'; Event.Char '4'; Event.Enter;
        Event.Right; Event.Enter; Event.Down; Event.Enter; Event.Down; Event.Enter ] in
  shows model "Last preview: d2d4"

let pieces () =
  let open Caravan_ui in
  let cases =
    [ Pawn, "♙", "♟"; Knight, "♘", "♞"; Bishop, "♗", "♝";
      Rook, "♖", "♜"; Queen, "♕", "♛"; King, "♔", "♚" ] in
  let previous = ref None in
  List.iter (fun (kind, white, black) ->
      List.iter (fun (colour, figure) ->
          let piece = make_piece kind colour in
          Alcotest.(check bool) "kind preserved" true (piece.kind = kind);
          Alcotest.(check bool) "colour preserved" true (piece.colour = colour);
          Alcotest.(check string) "chess figure" figure piece.figure;
          Option.iter (fun id ->
              Alcotest.(check int) "incrementing ID" (id + 1) piece.id) !previous;
          previous := Some piece.id)
        [ White, white; Black, black ]) cases

let board_figures () =
  let model = steps [ Event.Enter ] in
  List.iter (shows model)
    [ "♜  ♞  ♝  ♛  ♚  ♝  ♞  ♜";
      "♖  ♘  ♗  ♕  ♔  ♗  ♘  ♖"; "[♙]"; "♟" ];
  let before = Caravan_ui.make_piece Pawn White in
  ignore (Caravan_ui.view model);
  ignore (Caravan_ui.view (step model Event.Up));
  let after = Caravan_ui.make_piece Pawn White in
  Alcotest.(check int) "rendering does not create pieces" (before.id + 1) after.id;
  ignore (steps [ Event.Enter ]);
  let next = Caravan_ui.make_piece Pawn White in
  Alcotest.(check int) "new game creates 32 pieces" (after.id + 33) next.id

let frame_size () =
  List.iter (fun model ->
      let lines = String.split_on_char '\n' (Caravan_ui.view model) in
      Alcotest.(check bool) "fits 24 rows" true (List.length lines <= 24);
      List.iter (fun line ->
          Alcotest.(check bool) "fits 80 columns" true (String.fold_left (fun width c ->
               if Char.code c land 0xc0 = 0x80 then width else width + 1) 0 line < 80)) lines)
    [ Caravan_ui.initial; steps [ Event.Enter ];
      steps [ Event.Down; Event.Down; Event.Enter ] ]

let () =
  Alcotest.run "caravan"
    [ "views", [ Alcotest.test_case "landing and exit" `Quick landing;
                 Alcotest.test_case "difficulty and navigation" `Quick settings;
                 Alcotest.test_case "move previews" `Quick move_preview;
                 Alcotest.test_case "latest session save" `Quick save_load;
                 Alcotest.test_case "piece creation" `Quick pieces;
                 Alcotest.test_case "board figures" `Quick board_figures;
                 Alcotest.test_case "terminal size" `Quick frame_size ] ]
