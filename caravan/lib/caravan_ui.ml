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
  ; selected : int option
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
  ; saved : game option
  ; notice : string
  }

let initial =
  { screen = Landing 0; difficulty = Normal; saved = None; notice = "" }
;;

let new_game () =
  { state = State.initial ()
  ; square = 52
  ; selected = None
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
  ; selected = None
  ; input = ""
  ; notice = "Move preview only; engine and legal moves are not connected."
  }
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
  | Event.Backspace ->
    let n = String.length game.input in
    { game with input = String.sub game.input 0 (max 0 (n - 1)) }
  | key when confirm key ->
    if game.input <> ""
    then
      if Result.is_ok (Uci.move_of_string game.input)
      then preview game game.input
      else { game with notice = "Use a move such as e2e4 or e7e8q." }
    else (
      match game.selected with
      | None -> { game with selected = Some game.square }
      | Some square when square = game.square -> { game with selected = None }
      | Some square ->
        preview game (Uci.square_name square ^ Uci.square_name game.square))
  | Event.Char c when String.contains "abcdefgh12345678qrbn" c ->
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
              { model with screen = Play (new_game ()); notice = "" }, false
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
           { model with screen = Setting (min 3 (cursor + 1)) }, false
         | key when confirm key ->
           if cursor = 3
           then { model with screen = Landing 2 }, false
           else { model with difficulty = difficulties.(cursor) }, false
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

let setting cursor difficulty =
  "\n  Caravan / Setting\n\n  Difficulty\n\n"
  ^ String.concat
      "\n"
      (List.init 3 (fun i ->
         menu_item
           (cursor = i)
           ((if difficulty = difficulties.(i) then "(*) " else "( ) ")
            ^ difficulty_name difficulties.(i)))
       @ [ ""; menu_item (cursor = 3) "Back" ])
  ^ "\n\n  Difficulty will apply when the engine is connected."
  ^ "\n\n  Up/Down: choose   Enter/Space: confirm   Ctrl+C: quit\n"
;;

let play game difficulty =
  let side row =
    match row with
    | 0 -> "  Difficulty: " ^ difficulty_name difficulty
    | 1 -> "  White: You   Black: Stockfish"
    | 3 -> menu_item (game.focus = Save) "[ Save ]"
    | 4 -> menu_item (game.focus = Main_menu) "[ Main menu ]"
    | 6 -> "  Engine: not connected"
    | _ -> ""
  in
  let rows =
    List.init 8 (fun row ->
      let cells =
        List.init 8 (fun col ->
          let square = (row * 8) + col in
          let figure =
            match State.piece_at game.state square with
            | Some piece -> piece.figure
            | None -> "."
          in
          if game.focus = Board && game.square = square
          then Printf.sprintf "[%s]" figure
          else if game.selected = Some square
          then Printf.sprintf "{%s}" figure
          else Printf.sprintf " %s " figure)
      in
      Printf.sprintf
        "  %d  %s  %d%s"
        (8 - row)
        (String.concat "" cells)
        (8 - row)
        (side row))
  in
  "\n  Caravan / Play\n\n      a  b  c  d  e  f  g  h\n"
  ^ String.concat "\n" rows
  ^ "\n      a  b  c  d  e  f  g  h\n\n"
  ^ "  UCI move: "
  ^ game.input
  ^ "_\n"
  ^ "  Last preview: "
  ^ Option.value game.move ~default:"-"
  ^ "\n"
  ^ "  "
  ^ game.notice
  ^ "\n\n"
  ^ "  Arrows: move; Right from h-file: buttons; Left: board\n"
  ^ "  Enter/Space: select/place   Type: UCI   Backspace: edit\n"
  ^ "  Select the same square again to cancel. Ctrl+C: quit\n"
;;

let view model =
  match model.screen with
  | Landing cursor -> landing cursor model.notice
  | Play game -> play game model.difficulty
  | Setting cursor -> setting cursor model.difficulty
;;

let app =
  { init = initial, Cmd.none
  ; update
  ; view
  ; subscriptions = (fun _ -> Sub.keys (fun key -> Some key))
  }
;;
