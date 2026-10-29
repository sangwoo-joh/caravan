type 'a t = 'a Queue.t

let create = Queue.create
let push t x = Queue.push x t
let clear = Queue.clear
let is_empty = Queue.is_empty

let take t p =
  let rec find i next =
    match next () with
    | Seq.Nil -> None
    | Seq.Cons (x, rest) ->
      if p x then Some (i, x) else find (i + 1) rest
  in
  match find 0 (Queue.to_seq t) with
  | None -> None
  | Some (selected, x) ->
    for i = 0 to Queue.length t - 1 do
      let msg = Queue.pop t in
      if i <> selected then Queue.push msg t
    done;
    Some x
