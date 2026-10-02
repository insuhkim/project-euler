let solve n =
  let sq n = n * n in
  sq (n * (n + 1) / 2) - (n * (n + 1) * ((2 * n) + 1) / 6)
;;

let run _ = solve 100 |> string_of_int
