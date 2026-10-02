let nth_prime n =
  let limit =
    if n < 6
    then 15
    else (
      let n = float n in
      int_of_float (n *. (log n +. log (log n))))
  in
  let is_prime = Array.make (limit + 1) true in
  is_prime.(0) <- false;
  is_prime.(1) <- false;
  for p = 2 to int_of_float (sqrt (float limit)) do
    if is_prime.(p)
    then (
      let i = ref (p * p) in
      while !i <= limit do
        is_prime.(!i) <- false;
        i := !i + p
      done)
  done;
  let count = ref 0 in
  let result = ref 0 in
  for i = 2 to limit do
    if is_prime.(i)
    then (
      incr count;
      if !count = n then result := i)
  done;
  !result
;;

let solve = nth_prime
let run _ = solve 10_001 |> string_of_int
