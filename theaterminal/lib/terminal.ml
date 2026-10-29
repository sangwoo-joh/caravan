let original = ref None

let write s =
  output_string stdout s;
  flush stdout

let release () =
  match !original with
  | Some attr ->
    Unix.tcsetattr Unix.stdin Unix.TCSANOW attr;
    original := None;
    write "\x1b[?25h\x1b[?1049l"
  | None -> ()

let dimensions = ref (24, 80)
let size () = !dimensions

external query_size : Unix.file_descr -> int * int = "theaterminal_terminal_size"

let setup () =
  let attr = Unix.tcgetattr Unix.stdin in
  original := Some attr;
  Unix.tcsetattr Unix.stdin Unix.TCSANOW
    { attr with c_icanon = false; c_echo = false; c_isig = false; c_vmin = 1; c_vtime = 0 };
  at_exit release;
  write "\x1b[?1049h\x1b[?25l\x1b[2J\x1b[H";
  dimensions := query_size Unix.stdout

(* Redraw in place: home the cursor, overwrite each line clearing its tail with
   [ESC K], then [ESC J] to wipe any lines a taller previous frame left behind.
   Overwriting rather than clearing first avoids a blank flash between frames. *)
let render frame =
  let buf = Buffer.create (String.length frame + 64) in
  Buffer.add_string buf "\x1b[H";
  List.iteri
    (fun i line ->
      if i > 0 then Buffer.add_string buf "\r\n";
      Buffer.add_string buf line;
      Buffer.add_string buf "\x1b[K")
    (String.split_on_char '\n' frame);
  Buffer.add_string buf "\x1b[J";
  write (Buffer.contents buf)
