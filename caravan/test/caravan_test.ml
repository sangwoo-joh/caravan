let%expect_test "canonical path" =
  Caravan.Path.canonicalize "///home//me/..//other///" |> print_endline;
  [%expect "/home/other"]
;;
