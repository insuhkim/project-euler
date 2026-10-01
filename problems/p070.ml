(* let rec factors n k l =
  if k * k > n
  then if n = 1 then l else n :: l
  else if n mod k = 0
  then factors (n / k) k (k :: l)
  else factors n (k + 1) l
;;

let factor_list n = factors n 2 []
let rec pow n k = if k = 0 then 1 else n * pow n (k - 1)

let totient n =
  factor_list n
  |> List.fold_left
       (fun acc n ->
          match acc with
          | (i, count) :: tl when n = i -> (i, count + 1) :: tl
          | _ -> (n, 1) :: acc)
       []
  |> List.map (fun (p, i) -> pow p (i - 1) * (p - 1))
  |> List.fold_left ( * ) 1
;;

let digits n = string_of_int n |> String.to_seq |> List.of_seq |> List.sort compare
let is_same_permut n m = digits n = digits m

let rec loop n acc =
  if n <= 1
  then acc
  else (
    let phi = totient n in
    (if is_same_permut n phi then (n, phi) :: acc else acc) |> loop (n - 1))
;;

let () =
  loop 1_000_000 []
  |> List.fold_left
       (fun (n', phi') (n, phi) -> if n * phi' < n' * phi then n, phi else n', phi')
       (0, 1)
  |> fst
  |> print_int
;; *)
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
