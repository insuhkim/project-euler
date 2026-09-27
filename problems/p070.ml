let same_digits a b =
  let count n =
    let digits = Array.make 10 0 in
    let rec loop n =
      if n > 0
      then (
        digits.(n mod 10) <- digits.(n mod 10) + 1;
        loop (n / 10))
    in
    loop n;
    digits
  in
  count a = count b
;;

let () =
  let n = 10_000_000 in
  let phi = Array.init n Fun.id in
  for p = 2 to n - 1 do
    if phi.(p) = p
    then (
      let k = ref p in
      while !k < n do
        phi.(!k) <- phi.(!k) / p * (p - 1);
        k := !k + p
      done)
  done;
  let best = ref 0 in
  let best_phi = ref 1 in
  for i = 2 to n - 1 do
    let p = phi.(i) in
    if same_digits i p && (!best = 0 || i * !best_phi < !best * p)
    then (
      best := i;
      best_phi := p)
  done;
  print_int !best
;;
