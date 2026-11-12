type t

type search_result =
  { state : State.t
  ; bestmove : State.move option
  ; info : string list
  }

val start : string -> (t, string) result
val send : t -> Uci.command -> (unit, string) result

val receive
  :  ?pause:(float -> unit)
  -> ?timeout:float
  -> t
  -> (Uci.response, string) result

val close : ?pause:(float -> unit) -> t -> (Unix.process_status, string) result

val play_input
  :  ?skill:int
  -> ?movetime:int
  -> executable:string
  -> State.t
  -> string
  -> (search_result, string) result

val terminate : t -> unit
val wait : ?pause:(float -> unit) -> t -> (Unix.process_status, string) result

val search
  :  ?pause:(float -> unit)
  -> ?skill:int
  -> ?movetime:int
  -> t
  -> State.t
  -> (search_result, string) result

val analyse
  :  ?skill:int
  -> ?movetime:int
  -> executable:string
  -> State.t
  -> (search_result, string) result
