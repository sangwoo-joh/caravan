type t

type search_result =
  { state : State.t
  ; bestmove : State.move option
  ; info : string list
  }

val start : string -> (t, string) result
val send : t -> Uci.command -> (unit, string) result
val receive : ?timeout:float -> t -> (Uci.response, string) result
val close : t -> (Unix.process_status, string) result

val play_input
  :  ?movetime:int
  -> executable:string
  -> State.t
  -> string
  -> (search_result, string) result
