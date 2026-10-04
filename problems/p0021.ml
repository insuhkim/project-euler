let solve n =
  let arr = Array.make n 1 in
  for i = 2 to n - 1 do
    let rec loop j =
      if j >= n
      then (
        arr.(j) <- arr.(j) + i;
        loop (j + i))
    in
    loop (2 * i)
  done;
  let ans = ref 0 in
  Array.iteri (fun i d -> if d < n && i <> d && arr.(d) = i then ans := !ans + d) arr;
  !ans
;;

let run _ = solve 10_000 |> Int.to_string
