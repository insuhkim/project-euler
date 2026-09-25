let () =
  List.init 1000 Fun.id
  |> List.filter (fun n -> n mod 3 = 0 || n mod 5 = 0)
  |> List.fold_left ( + ) 0
  |> print_int
;;
