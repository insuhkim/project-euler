let solve digit =
  let d = Float.of_int digit in
  (d -. 1. +. (log10 5. /. 2.)) /. log10 ((1. +. sqrt 5.) /. 2.)
  |> Float.ceil
  |> Float.to_int
;;

let run _ = solve 1_000 |> Int.to_string
