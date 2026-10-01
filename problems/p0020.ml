open Z

let rec fact n = if n = 0 then one else of_int n * fact (Stdlib.( - ) n 1)

let solve n =
  fact n
  |> to_string
  |> String.to_seq
  |> Seq.map (fun c -> Stdlib.( - ) (Char.code c) (Char.code '0'))
  |> Seq.fold_left Stdlib.( + ) 0
;;

let run _ =
  assert (solve 10 = 27);
  solve 100 |> Int.to_string
;;
