type move

type command =
  | Uci
  | Isready
  | Ucinewgame
  | Position of move list
  | Go_movetime of int
  | Stop
  | Quit

type response =
  | Uciok
  | Readyok
  | Bestmove of move option
  | Info of string
  | Other of string

val square_name : int -> string
val move_of_string : string -> (move, string) result
val move_to_string : move -> string
val command_to_string : command -> (string, string) result
val response_of_string : string -> (response, string) result
