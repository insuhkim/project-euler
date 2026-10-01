let rec factors n k l =
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

let f n = float_of_int n /. float_of_int (totient n)

let rec loop n (maxn, maxt) =
  if n < 0
  then maxn
  else
    (let t = f n in
     if t > maxt then n, t else maxn, maxt)
    |> loop (n - 1)
;;

let () = loop 1_000_000 (-1, -1.) |> print_int
