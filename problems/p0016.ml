let solve n =
  Z.pow (Z.of_int 2) n
  |> Z.to_string
  |> String.to_seq
  |> Seq.map (fun c -> Char.code c - Char.code '0')
  |> Seq.fold_left ( + ) 0
;;

let run _ =
  assert (solve 15 = 26);
  solve 1000 |> Int.to_string
;;
