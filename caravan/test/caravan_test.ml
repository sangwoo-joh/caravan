open! Base
open Stdio

let%expect_test "hello" =
  print_endline "Hello";
  [%expect {|Hello|}]
;;
