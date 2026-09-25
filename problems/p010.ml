let () =
  let n = 2_000_000 in
  let prime = Array.make n true in
  prime.(1) <- false;
  for i = 2 to int_of_float (sqrt (float n)) do
    if prime.(i)
    then
      for j = i * i to n - 1 do
        if j mod i = 0 then prime.(j) <- false
      done
  done;
  let ans = ref 0 in
  Array.iteri (fun i b -> if b then ans := !ans + i) prime;
  print_int !ans
;;
