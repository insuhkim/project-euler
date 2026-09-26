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
    then
      for i = p * p to limit do
        if i mod p = 0 then is_prime.(i) <- false
      done
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

let () =
  assert (nth_prime 6 = 13);
  nth_prime 10_001 |> print_int
;;
