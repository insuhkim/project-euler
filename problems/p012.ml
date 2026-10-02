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

let solve n =
  let rec loop i =
    let a, b = if i mod 2 = 0 then i / 2, i + 1 else i, (i + 1) / 2 in
    let al, bl = factor_list a, factor_list b in
    let d = List.merge compare al bl |> num_divisors in
    if d > n then a * b else loop (i + 1)
  in
  loop 1
;;

let run _ =
  assert (solve 5 = 28);
  solve 500 |> string_of_int
;;
