let () =
  let n = 100 in
  let sq n = n * n in
  sq (n * (n + 1) / 2) - (n * (n + 1) * ((2 * n) + 1) / 6) |> print_int
;;
