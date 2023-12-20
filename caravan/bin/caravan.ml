open! Core

let build = Command.basic ~summary:"Build your site" (Command.Param.return (fun () -> print_endline "build"))

let command =
  Command.group
    ~summary:"Caravn is a static site generator in OCaml with tree-sitter syntax highlighter"
    [ "build", build ]
;;


let () =
  Command_unix.run command
