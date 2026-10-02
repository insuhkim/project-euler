let solve n =
  List.init n succ
  |> List.filter (fun n -> n mod 2 = 1)
  |> List.map (fun n -> n * n)
  |> List.fold_left ( + ) 0
;;

let run _ = solve 354_000 |> string_of_int
