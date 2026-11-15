type 'msg t =
  | None
  | Dispatch of 'msg
  | Quit
  | Batch of 'msg t list
  | Send : 'request Troupe.Address.t * 'request -> 'msg t

let none = None
let dispatch m = Dispatch m
let quit = Quit
let send address request = Send (address, request)
let batch cs = Batch cs

let rec run t ~dispatch ~on_quit =
  match t with
  | None -> ()
  | Dispatch m -> dispatch m
  | Quit -> on_quit ()
  | Send (address, request) -> Troupe.send address request
  | Batch cs -> List.iter (fun c -> run c ~dispatch ~on_quit) cs
;;
