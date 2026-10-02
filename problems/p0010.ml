let solve n =
  let prime = Array.make (n + 1) true in
  prime.(1) <- false;
  for i = 2 to int_of_float (sqrt (float n)) do
    if prime.(i)
    then (
      let j = ref (i * i) in
      while !j <= n do
        prime.(!j) <- false;
        j := !j + i
      done)
  done;
  let ans = ref 0 in
  Array.iteri (fun i b -> if b then ans := !ans + i) prime;
  !ans
;;

let run _ =
  assert (solve 10 = 17);
  solve 2_000_000 |> string_of_int
;;
