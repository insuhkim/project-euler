let solve n =
  List.init n Fun.id
  |> List.filter (fun n -> n mod 3 = 0 || n mod 5 = 0)
  |> List.fold_left ( + ) 0
;;

let run _ =
  assert (solve 10 = 23);
  solve 1_000 |> string_of_int
;;
