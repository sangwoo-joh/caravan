(** Caravan screens and keyboard navigation. *)

module Piece = Piece
module State = State
module Logic = Logic
module Uci = Uci
module Stockfish = Stockfish

(** The current screen, difficulty, and temporary session save. *)
type model

(** [initial] is the landing screen with normal difficulty selected. *)
val initial : model

(** [update_key key model] is the state and command after [key]. Ctrl+C requests
    termination. Legal moves update the board. Saves last only for the current run. *)
val update_key
  :  Theaterminal.KeyEvent.key
  -> model
  -> model * Theaterminal.KeyEvent.key Theaterminal.Cmd.t

(** [view model] is the terminal frame for [model]. *)
val view : model -> string

type message =
  | Key of Theaterminal.KeyEvent.key
  | Shake_tick
  | Engine_result of State.t * (State.move, string) result

val start_game : State.t -> model
val make_app : executable:string -> (model, message) Theaterminal.app

(** [app] is the interactive Caravan game. *)
val app : (model, message) Theaterminal.app
