let digit_sum n =
  Z.to_string n
  |> String.to_seq
  |> Seq.map (fun c -> Char.code c - Char.code '0')
  |> Seq.fold_left ( + ) 0
;;

let solve () =
  let ans = ref 0 in
  for a = 1 to 99 do
    for b = 1 to 99 do
      let s = digit_sum Z.(of_int a ** b) in
      if !ans < s then ans := s
    done
  done;
  !ans
;;

let run _ = solve () |> Int.to_string
