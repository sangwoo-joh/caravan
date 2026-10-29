type key =
  | Char of char
  | Enter
  | Tab
  | Backspace
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Unknown of string

let parse s =
  let n = String.length s in
  let rec sequence_end i =
    if i >= n then n
    else if s.[i] >= '\x40' && s.[i] <= '\x7e' then i + 1
    else if s.[i] >= '\x20' && s.[i] <= '\x3f' then sequence_end (i + 1)
    else i
  in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      match s.[i] with
      | '\r' | '\n' -> go (i + 1) (Enter :: acc)
      | '\t' -> go (i + 1) (Tab :: acc)
      | '\x7f' | '\b' -> go (i + 1) (Backspace :: acc)
      | '\x1b' when i + 1 < n && s.[i + 1] = '[' ->
        let next = sequence_end (i + 2) in
        let sequence = String.sub s i (next - i) in
        let key =
          match sequence with
          | "\x1b[A" -> Up
          | "\x1b[B" -> Down
          | "\x1b[C" -> Right
          | "\x1b[D" -> Left
          | _ -> Unknown sequence
        in
        go next (key :: acc)
      | '\x1b' -> go (i + 1) (Escape :: acc)
      | c when Char.code c >= 0x20 && Char.code c < 0x7f ->
        go (i + 1) (Char c :: acc)
      | c -> go (i + 1) (Unknown (String.make 1 c) :: acc)
  in
  go 0 []

let equal = ( = )

let pp fmt = function
  | Char c -> Format.fprintf fmt "Char %C" c
  | Enter -> Format.pp_print_string fmt "Enter"
  | Tab -> Format.pp_print_string fmt "Tab"
  | Backspace -> Format.pp_print_string fmt "Backspace"
  | Escape -> Format.pp_print_string fmt "Escape"
  | Up -> Format.pp_print_string fmt "Up"
  | Down -> Format.pp_print_string fmt "Down"
  | Left -> Format.pp_print_string fmt "Left"
  | Right -> Format.pp_print_string fmt "Right"
  | Unknown s -> Format.fprintf fmt "Unknown %S" s
