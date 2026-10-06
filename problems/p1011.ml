let solve n =
  let rec ( ^ ) n k = if k = 0 then 1 else n * (n ^ (k - 1)) in
  let rec f a b = if b = 1 then a else if a < b then f (b / a) a else f (a / b) b in
  let ans = ref Z.zero in
  for a = 2 to n - 1 do
    let x, i =
      let rec get_i n' i = if n' < a then n', i else get_i (n' / a) (i + 1) in
      get_i (n - 1) 0
    in
    let b' = if i = 1 then (n - 1) / a else a - 1
    and s = a * ((a ^ (i - 1)) - 1) / (a - 1) in
    List.init b' succ
    |> List.map (fun b ->
      let k = if b < x then s + (a ^ i) else if b > x then s else s + n - (x * (a ^ i)) in
      Z.of_int (k * f a b))
    |> List.fold_left Z.add !ans
    |> fun z -> ans := z
  done;
  Z.add Z.(of_int 2 * !ans) (Z.of_int (n * (n - 1) / 2))
;;

let run _ =
  assert (solve 10 = Z.of_int 343);
  assert (solve 100 = Z.of_int 269288);
  solve 3_000_000 |> Z.to_string
;;
