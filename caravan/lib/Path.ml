module List = struct
  include List

  let is_empty = function
    | [] -> true
    | _ -> false
  ;;
end

let make_lazy f =
  let raw = lazy (f ()) in
  fun () -> Lazy.force raw
;;

let path =
  make_lazy (fun () ->
    let path =
      try Unix.getenv "PATH" with
      | Not_found -> ""
    in
    String.split_on_char ':' path)
;;

let home = make_lazy (fun () -> Unix.getenv "HOME" (* do not catch Not_found exception here *))

(** Make path canonical. This implementation is from Python's posixpath.normpath *)
let canonicalize path =
  let empty = "" in
  let dir_cur = Filename.current_dir_name in
  let dir_parent = Filename.parent_dir_name in
  let dir_sep = '/' in
  (* Filename.dir_sep is string *)
  let is_relative = String.get path 0 <> dir_sep in
  let components = String.split_on_char dir_sep (String.trim path) in
  let new_components = ref [] in
  let rec loop = function
    | [] -> ()
    | comp :: tl ->
      if comp = dir_cur || comp = empty
      then ()
      else if comp <> dir_parent
              || (is_relative && List.is_empty !new_components)
              || ((not (List.is_empty !new_components)) && List.hd !new_components = dir_parent)
      then new_components := comp :: !new_components
      else if not (List.is_empty !new_components)
      then new_components := List.tl !new_components;
      loop tl
  in
  loop components;
  let canonical_form = List.rev !new_components |> String.concat Filename.dir_sep in
  if not is_relative
  then "/" ^ canonical_form
  else if canonical_form = empty
  then dir_cur
  else canonical_form
;;
