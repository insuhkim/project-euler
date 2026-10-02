let solve n =
  let rec loop (a, b) s =
    if b > n then s else loop (b, a + b) (if b mod 2 = 0 then s + b else s)
  in
  loop (1, 2) 0
;;

let run _ = solve 4_000_000 |> string_of_int
