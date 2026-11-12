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

let new_game user_colour =
  { state = State.initial ~user_colour ()
  ; square = (if user_colour = Piece.White then 52 else 12)
  ; input = ""
  ; move = None
  ; focus = Board
  ; notice = "Select a piece or type a move, e.g. e2e4."
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

let preview game move =
  { game with
    move = Some move
  ; state = State.think game.state
  ; input = ""
  ; notice = "Move preview only; engine and legal moves are not connected."
  }
;;

let select_or_place game =
  match State.action game.state with
  | Think ->
    (match State.pick game.state game.square with
     | Error notice -> { game with notice }
     | Ok state ->
       { game with
         state
       ; input = ""
       ; notice = "Choose a destination; select the same square to cancel."
       })
  | Pick square when square = game.square ->
    { game with
      state = State.think game.state
    ; notice = "Selection cancelled."
    }
  | Pick square ->
    preview game (Uci.square_name square ^ Uci.square_name game.square)
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
     | Ok move ->
       (match State.pick game.state move.from_square with
        | Error notice -> { game with notice }
        | Ok _ -> preview game (Uci.move_to_string move)))
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
                  screen = Play (new_game model.user_colour)
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
               | None -> " "
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
  String.concat
    "\n"
    ([ "  Caravan / Play"; files; border ]
     @ rows
     @ [ files
       ; "  UCI move: "
         ^ game.input
         ^ "_    Last preview: "
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

let app =
  { init = initial, Cmd.none
  ; update
  ; view
  ; subscriptions = (fun _ -> Sub.keys (fun key -> Some key))
  }
;;
