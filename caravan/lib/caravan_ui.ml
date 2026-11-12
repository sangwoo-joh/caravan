open Theaterminal
module Piece = Piece
module State = State
module Logic = Logic
module Uci = Uci
module Stockfish = Stockfish

type difficulty =
  | Easy
  | Normal
  | Hard

type focus =
  | Board
  | Save
  | Main_menu

type game =
  { state : State.t
  ; square : int
  ; input : string
  ; move : string option
  ; focus : focus
  ; notice : string
  ; shake : int
  }

type screen =
  | Landing of int
  | Play of game
  | Setting of int

type model =
  { screen : screen
  ; difficulty : difficulty
  ; user_colour : Piece.colour
  ; saved : game option
  ; notice : string
  }

let initial =
  { screen = Landing 0
  ; difficulty = Normal
  ; user_colour = Piece.White
  ; saved = None
  ; notice = ""
  }
;;

let new_game state =
  { state
  ; square = (if State.user_colour state = Piece.White then 52 else 12)
  ; input = ""
  ; move = None
  ; focus = Board
  ; notice = "Select a piece or type a move, e.g. e2e4."
  ; shake = 0
  }
;;

let start_game state =
  { initial with
    screen = Play (new_game state)
  ; user_colour = State.user_colour state
  }
;;

let difficulty_name = function
  | Easy -> "Easy"
  | Normal -> "Normal"
  | Hard -> "Hard"
;;

let difficulties = [| Easy; Normal; Hard |]

let confirm = function
  | Event.Enter | Event.Char ' ' -> true
  | _ -> false
;;

let reject_move game reason =
  { game with notice = "Cannot place there: " ^ reason; shake = 4 }
;;

let piece_name = function
  | Piece.Pawn -> "pawn"
  | Knight -> "knight"
  | Bishop -> "bishop"
  | Rook -> "rook"
  | Queen -> "queen"
  | King -> "king"
;;

let place game (move : State.move) =
  let result =
    Result.bind (State.pick game.state move.from_square) (fun _ ->
      Logic.play game.state move)
  in
  match result with
  | Error reason -> reject_move game reason
  | Ok state ->
    let captured =
      match State.piece_at game.state move.to_square with
      | Some piece -> Some piece
      | None ->
        (match State.piece_at game.state move.from_square with
         | Some piece
           when piece.kind = Pawn
                && move.from_square mod 8 <> move.to_square mod 8 ->
           State.piece_at
             game.state
             ((move.from_square / 8 * 8) + (move.to_square mod 8))
         | _ -> None)
    in
    let notice =
      match captured with
      | Some piece -> "Captured opponent's " ^ piece_name piece.kind ^ "."
      | None -> "Moved " ^ Uci.move_to_string move ^ "."
    in
    { game with
      state
    ; move = Some (Uci.move_to_string move)
    ; input = ""
    ; notice
    ; shake = 0
    }
;;

let select_or_place game =
  match State.action game.state with
  | Think ->
    (match State.pick game.state game.square with
     | Error notice -> { game with notice }
     | Ok _ when State.turn game.state <> State.user_colour game.state ->
       { game with notice = "It is not your turn." }
     | Ok state ->
       (match Logic.status state with
        | Checkmate _ | Draw _ -> { game with notice = "The game has ended." }
        | Playing | Check ->
          { game with
            state
          ; input = ""
          ; shake = 0
          ; notice = "Choose an x or an opponent's piece; same square cancels."
          }))
  | Pick square when square = game.square ->
    { game with
      state = State.think game.state
    ; notice = "Selection cancelled."
    ; shake = 0
    }
  | Pick square ->
    let promotion =
      match State.piece_at game.state square with
      | Some piece
        when piece.kind = Pawn && (game.square / 8 = 0 || game.square / 8 = 7)
        -> Some Piece.Queen
      | _ -> None
    in
    (match State.move ?promotion square game.square with
     | Error reason -> reject_move game reason
     | Ok move -> place game move)
;;

let board_key key game =
  let row = game.square / 8
  and col = game.square mod 8 in
  match key with
  | Event.Up -> { game with square = (max 0 (row - 1) * 8) + col }
  | Event.Down -> { game with square = (min 7 (row + 1) * 8) + col }
  | Event.Left -> { game with square = (row * 8) + max 0 (col - 1) }
  | Event.Right when col = 7 -> { game with focus = Save }
  | Event.Right -> { game with square = game.square + 1 }
  | Event.Backspace when State.action game.state = Think ->
    let n = String.length game.input in
    { game with input = String.sub game.input 0 (max 0 (n - 1)) }
  | Event.Enter when State.action game.state = Think && game.input <> "" ->
    (match Uci.move_of_string game.input with
     | Error notice -> { game with notice }
     | Ok move -> place game move)
  | key when confirm key -> select_or_place game
  | Event.Char c
    when State.action game.state = Think
         && String.contains "abcdefgh12345678qrbn" c ->
    if String.length game.input < 5
    then { game with input = game.input ^ String.make 1 c }
    else game
  | _ -> game
;;

let update key model =
  if key = Event.Unknown "\003"
  then model, Cmd.quit
  else (
    let model, quit =
      match model.screen with
      | Landing cursor ->
        (match key with
         | Event.Up ->
           { model with screen = Landing (max 0 (cursor - 1)) }, false
         | Event.Down ->
           { model with screen = Landing (min 3 (cursor + 1)) }, false
         | key when confirm key ->
           (match cursor with
            | 0 ->
              ( { model with
                  screen =
                    Play
                      (new_game
                         (State.initial ~user_colour:model.user_colour ()))
                ; notice = ""
                }
              , false )
            | 1 ->
              (match model.saved with
               | Some game ->
                 ( { model with
                     screen =
                       Play
                         { game with
                           focus = Board
                         ; notice =
                             "Loaded the latest session save (not a disk save)."
                         }
                   }
                 , false )
               | None ->
                 ( { model with
                     notice = "No session save yet. Start with Play."
                   }
                 , false ))
            | 2 ->
              let cursor =
                match model.difficulty with
                | Easy -> 0
                | Normal -> 1
                | Hard -> 2
              in
              { model with screen = Setting cursor; notice = "" }, false
            | _ -> model, true)
         | _ -> model, false)
      | Setting cursor ->
        (match key with
         | Event.Up ->
           { model with screen = Setting (max 0 (cursor - 1)) }, false
         | Event.Down ->
           { model with screen = Setting (min 5 (cursor + 1)) }, false
         | key when confirm key ->
           if cursor = 5
           then { model with screen = Landing 2 }, false
           else if cursor < 3
           then { model with difficulty = difficulties.(cursor) }, false
           else
             ( { model with
                 user_colour = (if cursor = 3 then Piece.White else Piece.Black)
               }
             , false )
         | _ -> model, false)
      | Play game ->
        (match game.focus, key with
         | Main_menu, key when confirm key ->
           { model with screen = Landing 0 }, false
         | Save, key when confirm key ->
           let game =
             { game with
               notice =
                 "Saved for this session only; disk saving is not connected."
             }
           in
           { model with screen = Play game; saved = Some game }, false
         | (Save | Main_menu), Event.Left ->
           { model with screen = Play { game with focus = Board } }, false
         | (Save | Main_menu), Event.Up ->
           { model with screen = Play { game with focus = Save } }, false
         | (Save | Main_menu), Event.Down ->
           { model with screen = Play { game with focus = Main_menu } }, false
         | Board, key ->
           { model with screen = Play (board_key key game) }, false
         | _ -> model, false)
    in
    model, if quit then Cmd.quit else Cmd.none)
;;

let menu_item selected label = (if selected then "  > " else "    ") ^ label

let landing cursor notice =
  let art =
    {|
                         Caravan

                     o   o   o
                      \ / \ /
                       |___|
                       / Q \
                      /_____\
                 ____/_______\____       __
            ____/                 \_____/  \_
       ____/     ~~~     ~~~                 _)
      /    \________________________       /
     /       |  |              |  | \_____/
             |  |              |  |
             |__|              |__|

|}
  in
  art
  ^ String.concat
      "\n"
      (List.mapi
         (fun i label -> menu_item (cursor = i) label)
         [ "Play"; "Load"; "Setting"; "Exit" ])
  ^ "\n  "
  ^ notice
  ^ "\n  Up/Down: choose   Enter/Space: confirm   Ctrl+C: quit\n"
;;

let setting cursor difficulty user_colour =
  "\n  Caravan / Setting\n\n  Difficulty\n\n"
  ^ String.concat
      "\n"
      (List.init 3 (fun i ->
         menu_item
           (cursor = i)
           ((if difficulty = difficulties.(i) then "(*) " else "( ) ")
            ^ difficulty_name difficulties.(i)))
       @ [ ""
         ; "  Your pieces"
         ; menu_item
             (cursor = 3)
             ((if user_colour = Piece.White then "(*) " else "( ) ") ^ "White")
         ; menu_item
             (cursor = 4)
             ((if user_colour = Piece.Black then "(*) " else "( ) ") ^ "Black")
         ; ""
         ; menu_item (cursor = 5) "Back"
         ])
  ^ "\n\n  Piece colour applies to new games."
  ^ "\n\n  Up/Down: choose   Enter/Space: confirm   Ctrl+C: quit\n"
;;

let play game difficulty =
  let destinations =
    match State.action game.state with
    | Think -> []
    | Pick source ->
      Logic.legal_moves game.state
      |> List.filter_map (fun (move : State.move) ->
        if move.from_square = source then Some move.to_square else None)
  in
  let selected =
    match State.action game.state with
    | Think -> None
    | Pick square -> Some square
  in
  let side row =
    match row with
    | 0 -> "  Difficulty: " ^ difficulty_name difficulty
    | 1 ->
      if State.user_colour game.state = Piece.White
      then "  White: You   Black: Stockfish"
      else "  White: Stockfish   Black: You"
    | 2 ->
      "  Action: "
      ^
        (match State.action game.state with
        | Think -> "think"
        | Pick _ -> "pick")
    | 3 -> menu_item (game.focus = Save) "[ Save ]"
    | 4 -> menu_item (game.focus = Main_menu) "[ Main menu ]"
    | 5 ->
      "  Turn: "
      ^ if State.turn game.state = Piece.White then "White" else "Black"
    | 6 -> "  Engine: not connected"
    | 7 -> "  [ ] cursor   { } picked"
    | _ -> ""
  in
  let border = "     +---+---+---+---+---+---+---+---+" in
  let files = "       a   b   c   d   e   f   g   h" in
  let rows =
    List.concat
      (List.init 8 (fun row ->
         let cells =
           List.init 8 (fun col ->
             let square = (row * 8) + col in
             let figure =
               match State.piece_at game.state square with
               | Some piece -> piece.figure
               | None -> if List.mem square destinations then "x" else " "
             in
             if selected = Some square
             then Printf.sprintf "{%s}" figure
             else if game.focus = Board && game.square = square
             then Printf.sprintf "[%s]" figure
             else Printf.sprintf " %s " figure)
         in
         [ Printf.sprintf
             "  %d  |%s| %d%s"
             (8 - row)
             (String.concat "|" cells)
             (8 - row)
             (side row)
         ; border
         ]))
  in
  let shift line =
    if game.shake = 0
    then line
    else if game.shake mod 2 = 0
    then String.sub line 1 (String.length line - 1)
    else " " ^ line
  in
  String.concat
    "\n"
    ([ "  Caravan / Play" ]
     @ List.map shift ([ files; border ] @ rows @ [ files ])
     @ [ "  UCI move: "
         ^ game.input
         ^ "_    Last move: "
         ^ Option.value game.move ~default:"-"
       ; "  " ^ game.notice
       ; "  Arrows: move; Right at h-file: menu; Left: board; Ctrl+C: quit"
       ; "  Enter: UCI/select/place; Space: select/place; Backspace: edit"
       ])
;;

let view model =
  match model.screen with
  | Landing cursor -> landing cursor model.notice
  | Play game -> play game model.difficulty
  | Setting cursor -> setting cursor model.difficulty model.user_colour
;;

type message =
  | Key of Event.key
  | Shake_tick

let app =
  { init = initial, Cmd.none
  ; update =
      (fun message model ->
        match message with
        | Key key -> update key model
        | Shake_tick ->
          let model =
            match model.screen with
            | Play game when game.shake > 0 ->
              { model with screen = Play { game with shake = game.shake - 1 } }
            | _ -> model
          in
          model, Cmd.none)
  ; view
  ; subscriptions =
      (fun model ->
        let keys = Sub.keys (fun key -> Some (Key key)) in
        match model.screen with
        | Play game when game.shake > 0 ->
          Sub.batch [ keys; Sub.every 0.06 (fun () -> Shake_tick) ]
        | _ -> keys)
  }
;;
