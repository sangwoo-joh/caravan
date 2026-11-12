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

(** [update key model] is the state and command after [key]. Ctrl+C requests
    termination. Legal moves update the board. Saves last only for the current run. *)
val update
  :  Theaterminal.Event.key
  -> model
  -> model * Theaterminal.Event.key Theaterminal.Cmd.t

(** [view model] is the terminal frame for [model]. *)
val view : model -> string

type message =
  | Key of Theaterminal.Event.key
  | Shake_tick

val start_game : State.t -> model

(** [app] is the interactive Caravan game. *)
val app : (model, message) Theaterminal.app
