let solve nums nth =
  let rec fact n = if n = 0 then 1 else n * fact (n - 1) in
  let rec split_nth n = function
    | [] -> failwith "out of bound"
    | x :: xs ->
      if n = 0
      then x, xs
      else (
        let x', xs' = split_nth (n - 1) xs in
        x', x :: xs')
  in
  let rec perm_nth ls = function
    | 0 -> ls
    | n ->
      let l = List.length ls in
      let f = fact (l - 1) in
      let k, n' = n / f, n mod f in
      let x, ls' = split_nth k ls in
      x :: perm_nth ls' n'
  in
  perm_nth nums (nth - 1)
;;

let run _ =
  let nums = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let nth = 1_000_000 in
  solve nums nth |> List.map Int.to_string |> String.concat ""
;;
