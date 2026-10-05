let solve () =
  let n = 28_123 in
  let abundants =
    let arr = Array.make n 1 in
    for i = 2 to n - 1 do
      let rec loop j =
        if j < n
        then (
          arr.(j) <- arr.(j) + i;
          loop (j + i))
      in
      loop (2 * i)
    done;
    let rec loop acc i =
      if i >= n
      then acc
      else if arr.(i) > i
      then loop (i :: acc) (i + 1)
      else loop acc (i + 1)
    in
    loop [] 1 |> List.rev
  in
  let is_sum = Array.make n false in
  let rec loop = function
    | [] -> ()
    | x :: xs as ls ->
      let rec loop' = function
        | y :: ys when y + x < n ->
          is_sum.(y + x) <- true;
          loop' ys
        | _ -> ()
      in
      loop' ls;
      loop xs
  in
  loop abundants;
  let ans = ref 0 in
  Array.iteri (fun i b -> if not b then ans := !ans + i) is_sum;
  !ans
;;

let run _ = solve () |> Int.to_string
