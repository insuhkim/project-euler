let solve d =
  let is_palindrome n =
    let s = string_of_int n in
    let l = String.length s in
    String.init l (fun i -> s.[l - i - 1]) = s
  in
  let largest = ref 0 in
  let rec pow n k = if k = 0 then 1 else n * pow n (k - 1) in
  let lo, hi = pow 10 (d - 1), pow 10 d - 1 in
  for i = hi downto lo do
    for j = i downto lo do
      let n = i * j in
      if n > !largest && is_palindrome n then largest := n
    done
  done;
  !largest
;;

let run _ = solve 3 |> string_of_int
