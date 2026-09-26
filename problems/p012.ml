let rec factors n k l =
  if k * k > n
  then if n = 1 then l else n :: l
  else if n mod k = 0
  then factors (n / k) k (k :: l)
  else factors n (k + 1) l
;;

let factor_list n = factors n 2 []

let num_divisors l =
  l
  |> List.fold_left
       (fun acc n ->
          match acc with
          | (i, count) :: tl when n = i -> (i, count + 1) :: tl
          | _ -> (n, 1) :: acc)
       []
  |> List.map (fun (_, i) -> i + 1)
  |> List.fold_left ( * ) 1
;;

let rec loop n =
  let a, b = if n mod 2 = 0 then n / 2, n + 1 else n, (n + 1) / 2 in
  let al, bl = factor_list a, factor_list b in
  let d = List.merge compare al bl |> num_divisors in
  if d > 500 then a * b else loop (n + 1)
;;

let () = loop 1 |> print_int
