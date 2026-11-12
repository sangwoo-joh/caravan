open Theaterminal

let step model key = fst (Caravan_ui.update key model)
let steps keys = List.fold_left step Caravan_ui.initial keys

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0
;;

let shows model text =
  Alcotest.(check bool) text true (contains (Caravan_ui.view model) text)
;;

let quit model key =
  let _, command = Caravan_ui.update key model in
  let quit = ref false in
  Cmd.run command ~dispatch:ignore ~on_quit:(fun () -> quit := true);
  !quit
;;

let landing () =
  List.iter
    (shows Caravan_ui.initial)
    [ "Caravan"; "> Play"; "Load"; "Setting"; "Exit" ];
  List.iter
    (fun key ->
       Alcotest.(check string)
         "unsupported input is ignored"
         (Caravan_ui.view Caravan_ui.initial)
         (Caravan_ui.view (step Caravan_ui.initial key)))
    [ Event.Char 'q'; Event.Escape; Event.Tab; Event.Char 'e' ];
  shows (steps [ Event.Down; Event.Enter ]) "No session save yet";
  let exit = steps [ Event.Down; Event.Down; Event.Down ] in
  Alcotest.(check bool) "Exit confirms" true (quit exit Event.Enter);
  Alcotest.(check bool)
    "Ctrl+C quits"
    true
    (quit Caravan_ui.initial (Event.Unknown "\003"))
;;

let settings () =
  let model =
    steps [ Event.Down; Event.Down; Event.Enter; Event.Down; Event.Char ' ' ]
  in
  shows model "(*) Hard";
  let model =
    List.fold_left
      step
      model
      [ Event.Down
      ; Event.Down
      ; Event.Down
      ; Event.Enter
      ; Event.Up
      ; Event.Up
      ; Event.Enter
      ]
  in
  shows model "Caravan / Play";
  shows model "Difficulty: Hard";
  Alcotest.(check bool)
    "Ctrl+C quits play"
    true
    (quit model (Event.Unknown "\003"))
;;

let piece_movement () =
  let model =
    steps [ Event.Enter; Event.Enter; Event.Up; Event.Up; Event.Char ' ' ]
  in
  shows model "Last move: e2e4";
  shows model "Moved e2e4.";
  shows model "Turn: Black";
  let model =
    List.fold_left
      step
      model
      [ Event.Char 'e'
      ; Event.Char '2'
      ; Event.Char 'e'
      ; Event.Char '4'
      ; Event.Char 'r'
      ; Event.Backspace
      ; Event.Enter
      ]
  in
  shows model "Last move: e2e4";
  let model = List.fold_left step model [ Event.Char 'e'; Event.Enter ] in
  shows model "Use a move such as e2e4";
  shows model "Last move: e2e4"
;;

let save_load () =
  let model =
    steps
      [ Event.Enter
      ; Event.Enter
      ; Event.Up
      ; Event.Up
      ; Event.Enter
      ; Event.Right
      ; Event.Right
      ; Event.Right
      ; Event.Right
      ; Event.Enter
      ; Event.Down
      ; Event.Enter
      ; Event.Enter
      ]
  in
  shows model "Last move: -";
  let model =
    List.fold_left
      step
      model
      [ Event.Right
      ; Event.Right
      ; Event.Right
      ; Event.Right
      ; Event.Down
      ; Event.Enter
      ; Event.Down
      ; Event.Enter
      ]
  in
  shows model "Last move: e2e4";
  shows model "Loaded the latest session save";
  let model =
    List.fold_left
      step
      model
      [ Event.Char 'd'
      ; Event.Char '2'
      ; Event.Char 'd'
      ; Event.Char '4'
      ; Event.Enter
      ; Event.Right
      ; Event.Enter
      ; Event.Down
      ; Event.Enter
      ; Event.Down
      ; Event.Enter
      ]
  in
  shows model "Last move: e2e4"
;;

let pieces () =
  let open Caravan_ui.Piece in
  let cases =
    [ Pawn, "♙", "♟"
    ; Knight, "♘", "♞"
    ; Bishop, "♗", "♝"
    ; Rook, "♖", "♜"
    ; Queen, "♕", "♛"
    ; King, "♔", "♚"
    ]
  in
  let previous = ref None in
  List.iter
    (fun (kind, white, black) ->
       List.iter
         (fun (colour, figure) ->
            let piece = make_piece kind colour in
            Alcotest.(check bool) "kind preserved" true (piece.kind = kind);
            Alcotest.(check bool) "colour preserved" true (piece.colour = colour);
            Alcotest.(check string) "chess figure" figure piece.figure;
            Option.iter
              (fun id ->
                 Alcotest.(check int) "incrementing ID" (id + 1) piece.id)
              !previous;
            previous := Some piece.id)
         [ White, white; Black, black ])
    cases
;;

let board_figures () =
  let model = steps [ Event.Enter ] in
  List.iter
    (shows model)
    [ "| ♜ | ♞ | ♝ | ♛ | ♚ | ♝ | ♞ | ♜ |"
    ; "| ♖ | ♘ | ♗ | ♕ | ♔ | ♗ | ♘ | ♖ |"
    ; "+---+---+---+---+---+---+---+---+"
    ; "|[♙]|"
    ; "♟"
    ];
  let before = Caravan_ui.Piece.make_piece Pawn White in
  ignore (Caravan_ui.view model);
  ignore (Caravan_ui.view (step model Event.Up));
  let after = Caravan_ui.Piece.make_piece Pawn White in
  Alcotest.(check int)
    "rendering does not create pieces"
    (before.id + 1)
    after.id;
  ignore (steps [ Event.Enter ]);
  let next = Caravan_ui.Piece.make_piece Pawn White in
  Alcotest.(check int) "new game creates 32 pieces" (after.id + 33) next.id
;;

let type_input model text =
  String.fold_left (fun model c -> step model (Event.Char c)) model text
;;

let repeat key count model =
  List.fold_left step model (List.init count (fun _ -> key))
;;

let selection () =
  let start = steps [ Event.Enter ] in
  shows start "Action: think";
  shows start "White: You   Black: Stockfish";
  let empty = step (step start Event.Up) (Event.Char ' ') in
  shows empty "Action: think";
  shows empty "Select a square containing your piece.";
  let opponent = step (repeat Event.Up 5 start) Event.Enter in
  shows opponent "You can only select your own pieces.";
  shows opponent "Action: think";
  let picked = step (type_input start "e2") (Event.Char ' ') in
  shows picked "Action: pick";
  shows picked "UCI move: _";
  shows picked "|{♙}|";
  let ignored = step (type_input picked "e7e5") Event.Backspace in
  Alcotest.(check string)
    "pick ignores UCI input"
    (Caravan_ui.view picked)
    (Caravan_ui.view ignored);
  let placed = step (step picked Event.Up) Event.Enter in
  shows placed "Action: think";
  shows placed "Last move: e2e3";
  shows (type_input placed "d2") "UCI move: d2_";
  let cancelled = step picked (Event.Char ' ') in
  shows cancelled "Action: think";
  shows cancelled "Selection cancelled.";
  shows (type_input cancelled "e2") "UCI move: e2_";
  let rejected = step (type_input start "e7e5") Event.Enter in
  shows rejected "You can only select your own pieces.";
  shows rejected "Last move: -";
  shows rejected "Action: think";
  let empty_input = step (type_input start "e3e4") Event.Enter in
  shows empty_input "Select a square containing your piece.";
  shows empty_input "Last move: -"
;;

let black_game () =
  steps
    [ Event.Down
    ; Event.Down
    ; Event.Enter
    ; Event.Down
    ; Event.Down
    ; Event.Down
    ; Event.Enter
    ; Event.Down
    ; Event.Enter
    ; Event.Up
    ; Event.Up
    ; Event.Enter
    ]
;;

let piece_colour () =
  let settings = steps [ Event.Down; Event.Down; Event.Enter ] in
  shows settings "(*) White";
  shows settings "( ) Black";
  let black = black_game () in
  shows black "White: Stockfish   Black: You";
  shows black "|[♟]|";
  shows (step black Event.Enter) "It is not your turn.";
  let opponent = step (repeat Event.Up 5 black) Event.Enter in
  shows opponent "You can only select your own pieces.";
  shows opponent "Action: think";
  let rejected = step (type_input black "e2e4") Event.Enter in
  shows rejected "You can only select your own pieces.";
  shows (step (type_input black "e7e5") Event.Enter) "Cannot place there:";
  let saved =
    repeat Event.Right 5 black |> fun model -> step model Event.Enter
  in
  let landing = List.fold_left step saved [ Event.Down; Event.Enter ] in
  let white =
    List.fold_left
      step
      landing
      [ Event.Down
      ; Event.Down
      ; Event.Enter
      ; Event.Down
      ; Event.Down
      ; Event.Enter
      ; Event.Down
      ; Event.Down
      ; Event.Enter
      ; Event.Up
      ; Event.Up
      ; Event.Enter
      ]
  in
  shows white "White: You   Black: Stockfish";
  let loaded =
    repeat Event.Right 4 white
    |> fun model ->
    List.fold_left
      step
      model
      [ Event.Down; Event.Enter; Event.Down; Event.Enter ]
  in
  shows loaded "White: Stockfish   Black: You";
  shows (step loaded Event.Enter) "It is not your turn."
;;

let board_boxes () =
  let lines =
    String.split_on_char '\n' (Caravan_ui.view (steps [ Event.Enter ]))
  in
  let borders =
    List.filter
      (fun line -> contains line "+---+---+---+---+---+---+---+---+")
      lines
  in
  Alcotest.(check int) "nine horizontal borders" 9 (List.length borders);
  let rows = List.filter (fun line -> String.contains line '|') lines in
  Alcotest.(check int) "eight ranks" 8 (List.length rows);
  List.iter
    (fun line ->
       Alcotest.(check int)
         "nine vertical borders per rank"
         9
         (String.fold_left
            (fun count c -> count + if c = '|' then 1 else 0)
            0
            line))
    rows
;;

let cell model square =
  let row = square / 8
  and col = square mod 8 in
  let rows =
    String.split_on_char '\n' (Caravan_ui.view model)
    |> List.filter (fun line -> String.contains line '|')
  in
  List.nth (String.split_on_char '|' (List.nth rows row)) (col + 1)
;;

let move_cursor model source target =
  let vertical = (target / 8) - (source / 8)
  and horizontal = (target mod 8) - (source mod 8) in
  model
  |> repeat (if vertical < 0 then Event.Up else Event.Down) (abs vertical)
  |> repeat
       (if horizontal < 0 then Event.Left else Event.Right)
       (abs horizontal)
;;

let marked model =
  List.init 64 Fun.id
  |> List.filter (fun square -> String.contains (cell model square) 'x')
;;

let fixture ?en_passant pieces =
  let board = Array.make 64 None in
  List.iter
    (fun (square, kind, colour) ->
       board.(square) <- Some (Caravan_ui.Piece.make_piece kind colour))
    pieces;
  match Caravan_ui.State.create ?en_passant ~turn:White board with
  | Ok state -> Caravan_ui.start_game state
  | Error error -> Alcotest.fail error
;;

let destinations () =
  let picked = steps [ Event.Enter; Event.Enter ] in
  Alcotest.(check (list int)) "pawn forward squares" [ 36; 44 ] (marked picked);
  let invalid = step (move_cursor picked 52 28) Event.Enter in
  shows invalid "Cannot place there:";
  shows invalid "Action: pick";
  shows invalid "Turn: White";
  shows invalid "Last move: -";
  Alcotest.(check (list int))
    "invalid placement keeps targets"
    [ 36; 44 ]
    (marked invalid);
  Alcotest.(check string) "pawn stays at source" "{♙}" (cell invalid 52);
  let placed = step (move_cursor invalid 28 36) Event.Enter in
  shows placed "Moved e2e4.";
  shows placed "Turn: Black";
  shows placed "Action: think";
  Alcotest.(check (list int))
    "targets cleared after placement"
    []
    (marked placed);
  Alcotest.(check string) "source cleared" "   " (cell placed 52);
  Alcotest.(check string) "pawn placed" "[♙]" (cell placed 36);
  shows (step placed Event.Enter) "It is not your turn.";
  let cancelled = step picked Event.Enter in
  Alcotest.(check (list int)) "cancel clears targets" [] (marked cancelled);
  let knight =
    steps [ Event.Enter; Event.Down; Event.Right; Event.Right; Event.Enter ]
  in
  Alcotest.(check (list int))
    "knight jumps over pieces"
    [ 45; 47 ]
    (marked knight);
  let pinned =
    fixture [ 60, King, White; 0, King, Black; 52, Rook, White; 4, Rook, Black ]
    |> fun model -> step model Event.Enter
  in
  Alcotest.(check (list int))
    "pinned rook cannot expose king"
    [ 12; 20; 28; 36; 44 ]
    (marked pinned)
;;

let capture () =
  let start =
    fixture
      [ 56, King, White; 7, King, Black; 52, Pawn, White; 43, Rook, Black ]
  in
  let picked = step start Event.Enter in
  Alcotest.(check string)
    "capture target keeps enemy figure"
    " ♜ "
    (cell picked 43);
  Alcotest.(check (list int))
    "only empty destinations marked"
    [ 36; 44 ]
    (marked picked);
  let captured = step (move_cursor picked 52 43) Event.Enter in
  shows captured "Captured opponent's rook.";
  shows captured "Turn: Black";
  shows captured "Action: think";
  Alcotest.(check string) "captured rook replaced" "[♙]" (cell captured 43);
  Alcotest.(check string) "source cleared" "   " (cell captured 52);
  Alcotest.(check (list int)) "capture clears markers" [] (marked captured);
  let typed = step (type_input start "e2d3") Event.Enter in
  shows typed "Captured opponent's rook.";
  Alcotest.(check string) "typed capture applied" " ♙ " (cell typed 43);
  let ep =
    fixture
      ~en_passant:19
      [ 56, King, White; 7, King, Black; 28, Pawn, White; 27, Pawn, Black ]
  in
  let picked = step (move_cursor ep 52 28) Event.Enter in
  Alcotest.(check bool)
    "empty en passant target marked"
    true
    (List.mem 19 (marked picked));
  let captured = step (move_cursor picked 28 19) Event.Enter in
  shows captured "Captured opponent's pawn.";
  Alcotest.(check string) "en passant victim removed" "   " (cell captured 27);
  Alcotest.(check string) "en passant pawn placed" "[♙]" (cell captured 19)
;;

let promotion () =
  let start = fixture [ 56, King, White; 7, King, Black; 12, Pawn, White ] in
  let picked = step (move_cursor start 52 12) Event.Enter in
  let promoted = step (step picked Event.Up) Event.Enter in
  shows promoted "Last move: e7e8q";
  Alcotest.(check string)
    "click promotion defaults to queen"
    "[♕]"
    (cell promoted 4);
  let typed = step (type_input start "e7e8n") Event.Enter in
  Alcotest.(check string) "typed underpromotion" " ♘ " (cell typed 4)
;;

let shake () =
  let picked = steps [ Event.Enter; Event.Enter ] in
  let rejected = step (step picked Event.Left) Event.Enter in
  let has_timer model =
    List.exists
      (fun (_, source) ->
         match source with
         | Sub.Every _ -> true
         | _ -> false)
      (Sub.leaves (Caravan_ui.app.subscriptions model))
  in
  Alcotest.(check bool)
    "invalid placement starts timer"
    true
    (has_timer rejected);
  let tick model = fst (Caravan_ui.app.update Caravan_ui.Shake_tick model) in
  let shifted = tick rejected in
  Alcotest.(check bool)
    "shake changes board position"
    true
    (Caravan_ui.view shifted <> Caravan_ui.view rejected);
  let settled = tick (tick (tick shifted)) in
  Alcotest.(check bool) "shake stops automatically" false (has_timer settled);
  shows settled "Action: pick";
  shows settled "Cannot place there:";
  shows settled "     +---+";
  Alcotest.(check (list int))
    "shake preserves targets"
    [ 36; 44 ]
    (marked settled)
;;

let black_orientation () =
  let black = black_game () in
  shows black "h   g   f   e   d   c   b   a";
  shows black "  1  | ♖ | ♘ | ♗ | ♔ | ♕ | ♗ | ♘ | ♖ | 1";
  shows black "  8  | ♜ | ♞ | ♝ | ♚ | ♛ | ♝ | ♞ | ♜ | 8";
  Alcotest.(check string) "black cursor starts on e7" "[♟]" (cell black 51);
  let right_edge = repeat Event.Right 5 black in
  shows right_edge "> [ Save ]";
  let back = step right_edge Event.Left in
  Alcotest.(check string) "menu returns to a7" "[♟]" (cell back 55);
  let state = Caravan_ui.State.initial ~user_colour:Black () in
  let white_move =
    match Caravan_ui.Uci.move_of_string "e2e4" with
    | Ok move -> move
    | Error error -> Alcotest.fail error
  in
  let state =
    match Caravan_ui.Logic.play state white_move with
    | Ok state -> state
    | Error error -> Alcotest.fail error
  in
  let model = Caravan_ui.start_game state in
  let picked = step model Event.Enter in
  Alcotest.(check (list int))
    "black pawn targets point upward"
    [ 35; 43 ]
    (marked picked);
  let moved = List.fold_left step picked [ Event.Up; Event.Up; Event.Enter ] in
  shows moved "Last move: e7e5";
  shows moved "Turn: White";
  Alcotest.(check string) "black pawn placed at e5" "[♟]" (cell moved 35);
  Alcotest.(check string) "e7 cleared" "   " (cell moved 51);
  let left = step model Event.Left in
  Alcotest.(check string) "left goes toward f-file" "[♟]" (cell left 50);
  let down = step model Event.Down in
  Alcotest.(check string) "down goes toward rank 8" "[♚]" (cell down 59)
;;

let with_engine reply run =
  let script = Filename.temp_file "caravan-ui-engine-" ".sh" in
  let log = Filename.temp_file "caravan-ui-engine-" ".log" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove script;
      Sys.remove log)
    (fun () ->
       let output = open_out script in
       output_string
         output
         ("#!/bin/sh\nprintf 'pid %s\\n' \"$$\" >> "
          ^ Filename.quote log
          ^ "\nwhile IFS= read -r line; do\n"
          ^ "printf '%s\\n' \"$line\" >> "
          ^ Filename.quote log
          ^ "\n"
          ^ "case \"$line\" in\nuci) printf 'uciok\\n';;\n"
          ^ "isready) printf 'readyok\\n';;\ngo*) sleep 0.05; "
          ^ reply
          ^ ";;\n"
          ^ "quit) exit 0;;\nesac\ndone\n");
       close_out output;
       Unix.chmod script 0o700;
       run script log)
;;

let run_engine executable initial =
  let base = Caravan_ui.make_app ~executable in
  let last = ref initial
  and ticks = ref 0
  and finished = ref false in
  let app =
    { base with
      init = initial, Cmd.none
    ; update =
        (fun message model ->
          let next, command = base.update message model in
          last := next;
          match message with
          | Caravan_ui.Engine_result _ ->
            finished := true;
            next, Cmd.batch [ command; Cmd.quit ]
          | Shake_tick ->
            incr ticks;
            next, if !ticks > 1000 then Cmd.quit else command
          | _ -> next, command)
    ; subscriptions =
        (fun model ->
          let sources =
            Sub.leaves (base.subscriptions model)
            |> List.filter_map (fun (key, source) ->
              match source with
              | Sub.Keys _ -> None
              | Sub.Every (seconds, tick) -> Some (Sub.every ~key seconds tick)
              | Sub.Worker (run, on_stop) -> Some (Sub.worker ~key ~on_stop run))
          in
          Sub.batch
            (Sub.every ~key:"test-heartbeat" 0.01 (fun () ->
               Caravan_ui.Shake_tick)
             :: sources))
    }
  in
  run_headless ~render:ignore app;
  Alcotest.(check bool) "engine finishes" true !finished;
  !last, !ticks
;;

let engine_turn () =
  let start = steps [ Event.Enter ] in
  let waiting = step (type_input start "e2e4") Event.Enter in
  shows waiting "Engine: thinking (1s)";
  with_engine "printf 'bestmove e7e5\\n'" (fun executable log ->
    let replied, ticks = run_engine executable waiting in
    shows replied "PC moved e7e5.";
    shows replied "Turn: White";
    shows replied "Engine: ready";
    Alcotest.(check string) "engine pawn applied" " ♟ " (cell replied 28);
    Alcotest.(check bool) "UI ticks during search" true (ticks >= 2);
    let commands = In_channel.with_open_bin log In_channel.input_all in
    List.iter
      (fun command ->
         Alcotest.(check bool) command true (contains commands command))
      [ "setoption name UCI_LimitStrength value false\n"
      ; "setoption name Skill Level value 5\n"
      ; "go movetime 1000\n"
      ; "moves e2e4\n"
      ; "quit\n"
      ];
    let second = step (type_input replied "g1f3") Event.Enter in
    shows second "Engine: thinking (1s)");
  with_engine "printf 'bestmove e2e4\\n'" (fun executable _ ->
    let replied, _ = run_engine executable (black_game ()) in
    shows replied "PC moved e2e4.";
    shows replied "Turn: Black";
    shows replied "Engine: ready";
    Alcotest.(check string)
      "white first move on rotated board"
      " ♙ "
      (cell replied 27));
  with_engine "printf 'bestmove e7e4\\n'" (fun executable _ ->
    let failed, _ = run_engine executable waiting in
    shows failed "Engine: error";
    shows failed "Engine error:";
    shows failed "Last move: e2e4";
    Alcotest.(check string)
      "illegal engine move not applied"
      " ♟ "
      (cell failed 12))
;;

let difficulty_mapping () =
  List.iter
    (fun (cursor, level) ->
       let settings = steps [ Event.Down; Event.Down; Event.Enter; Event.Up ] in
       let selected =
         repeat Event.Down cursor settings
         |> fun model -> step model Event.Enter
       in
       let start =
         repeat Event.Down (5 - cursor) selected
         |> fun model ->
         List.fold_left
           step
           model
           [ Event.Enter; Event.Up; Event.Up; Event.Enter ]
       in
       let waiting = step (type_input start "e2e4") Event.Enter in
       with_engine "printf 'bestmove e7e5\\n'" (fun executable log ->
         ignore (run_engine executable waiting);
         let commands = In_channel.with_open_bin log In_channel.input_all in
         Alcotest.(check bool)
           "selected skill sent"
           true
           (contains
              commands
              (Printf.sprintf "setoption name Skill Level value %d\n" level))))
    [ 0, 0; 1, 5; 2, 10 ]
;;

let stale_engine_result () =
  let state = Caravan_ui.State.initial ~user_colour:Black () in
  let model = Caravan_ui.start_game state in
  let move =
    match Caravan_ui.Uci.move_of_string "e2e4" with
    | Ok move -> move
    | Error e -> Alcotest.fail e
  in
  let different = Caravan_ui.State.initial ~user_colour:Black () in
  let next, _ =
    Caravan_ui.app.update (Engine_result (different, Ok move)) model
  in
  Alcotest.(check string)
    "stale result ignored"
    (Caravan_ui.view model)
    (Caravan_ui.view next);
  let next, _ = Caravan_ui.app.update (Engine_result (state, Ok move)) model in
  shows next "PC moved e2e4."
;;

let cancel_engine ?(quit = false) () =
  with_engine ":" (fun executable log ->
    let base = Caravan_ui.make_app ~executable in
    let waiting =
      steps [ Event.Enter ]
      |> fun model -> step (type_input model "e2e4") Event.Enter
    in
    let departed_at = ref None in
    let ticks = ref 0
    and replies = ref 0
    and last = ref waiting in
    let app =
      { base with
        init = waiting, Cmd.none
      ; update =
          (fun message model ->
            let model, command = base.update message model in
            let model =
              match message with
              | Caravan_ui.Shake_tick ->
                incr ticks;
                let commands =
                  In_channel.with_open_bin log In_channel.input_all
                in
                if !departed_at = None && contains commands "go movetime 1000"
                then (
                  departed_at := Some !ticks;
                  if quit
                  then model
                  else
                    List.fold_left
                      step
                      model
                      [ Event.Right
                      ; Event.Right
                      ; Event.Right
                      ; Event.Right
                      ; Event.Down
                      ; Event.Enter
                      ])
                else model
              | Engine_result _ ->
                incr replies;
                model
              | _ -> model
            in
            last := model;
            ( model
            , if
                !ticks >= 500
                || (quit && !departed_at <> None)
                || Option.fold
                     ~none:false
                     ~some:(fun n -> !ticks >= n + 5)
                     !departed_at
              then Cmd.quit
              else command ))
      ; subscriptions =
          (fun model ->
            let workers =
              Sub.leaves (base.subscriptions model)
              |> List.filter_map (fun (key, source) ->
                match source with
                | Sub.Worker (run, on_stop) ->
                  Some (Sub.worker ~key ~on_stop run)
                | _ -> None)
            in
            Sub.batch
              (Sub.every 0.02 (fun () -> Caravan_ui.Shake_tick) :: workers))
      }
    in
    run_headless ~render:ignore app;
    if not quit then shows !last "> Play";
    Alcotest.(check int) "cancelled search never replies" 0 !replies;
    let commands = In_channel.with_open_bin log In_channel.input_all in
    Alcotest.(check bool)
      "search had started"
      true
      (contains commands "go movetime 1000");
    let pid_line = List.hd (String.split_on_char '\n' commands) in
    let pid = Scanf.sscanf pid_line "pid %d" Fun.id in
    let reaped =
      try
        ignore (Unix.waitpid [ Unix.WNOHANG ] pid);
        false
      with
      | Unix.Unix_error (Unix.ECHILD, _, _) -> true
    in
    Alcotest.(check bool)
      "engine process reaped before runtime returns"
      true
      reaped)
;;

let frame_size () =
  List.iter
    (fun model ->
       let lines = String.split_on_char '\n' (Caravan_ui.view model) in
       Alcotest.(check bool) "fits 24 rows" true (List.length lines <= 24);
       List.iter
         (fun line ->
            Alcotest.(check bool)
              "fits 80 columns"
              true
              (String.fold_left
                 (fun width c ->
                    if Char.code c land 0xc0 = 0x80 then width else width + 1)
                 0
                 line
               < 80))
         lines)
    [ Caravan_ui.initial
    ; steps [ Event.Enter ]
    ; steps [ Event.Down; Event.Down; Event.Enter ]
    ; black_game ()
    ; steps [ Event.Enter; Event.Enter ]
    ; steps [ Event.Enter; Event.Enter; Event.Left; Event.Enter ]
    ]
;;

let () =
  Alcotest.run
    "caravan"
    [ ( "views"
      , [ Alcotest.test_case "landing and exit" `Quick landing
        ; Alcotest.test_case "difficulty and navigation" `Quick settings
        ; Alcotest.test_case "piece movement" `Quick piece_movement
        ; Alcotest.test_case "latest session save" `Quick save_load
        ; Alcotest.test_case "piece creation" `Quick pieces
        ; Alcotest.test_case "board figures" `Quick board_figures
        ; Alcotest.test_case "selection and input mode" `Quick selection
        ; Alcotest.test_case "player colour" `Quick piece_colour
        ; Alcotest.test_case "board boxes" `Quick board_boxes
        ; Alcotest.test_case
            "legal destinations and placement"
            `Quick
            destinations
        ; Alcotest.test_case "captures" `Quick capture
        ; Alcotest.test_case "promotion" `Quick promotion
        ; Alcotest.test_case "invalid placement shake" `Quick shake
        ; Alcotest.test_case "black orientation" `Quick black_orientation
        ; Alcotest.test_case "engine turn" `Quick engine_turn
        ; Alcotest.test_case "difficulty mapping" `Quick difficulty_mapping
        ; Alcotest.test_case "stale engine result" `Quick stale_engine_result
        ; Alcotest.test_case
            "cancel engine search"
            `Quick
            (cancel_engine ~quit:false)
        ; Alcotest.test_case
            "quit during engine search"
            `Quick
            (cancel_engine ~quit:true)
        ; Alcotest.test_case "terminal size" `Quick frame_size
        ] )
    ]
;;
