type 'msg source =
  | Keys of (KeyEvent.key -> 'msg option)
  | Every of float * (unit -> 'msg)
  | Worker :
      ('request Troupe.Actor.t -> dispatch:('msg -> unit) -> unit) * (unit -> unit)
      -> 'msg source

type 'msg t =
  | None
  | Leaf of string * 'msg source
  | Batch of 'msg t list

let none = None
let keys f = Leaf ("keys", Keys f)

let every ?key seconds f =
  let key =
    match key with
    | Some k -> "every:key:" ^ k
    | None -> Printf.sprintf "every:interval:%.17g" seconds
  in
  Leaf (key, Every (seconds, f))
;;

let worker ~key ~cleanup run = Leaf ("worker:" ^ key, Worker (run, cleanup))
let batch subs = Batch subs

let rec leaves = function
  | None -> []
  | Leaf (key, source) -> [ key, source ]
  | Batch subs -> List.concat_map leaves subs
;;
