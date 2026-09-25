let () =
  List.init 354_000 (fun i -> i + 1)
  |> List.filter_map (fun n -> if n mod 2 = 0 then None else Some (n * n))
  |> List.fold_left ( + ) 0
  |> print_int
;;
