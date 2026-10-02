let solve () =
  let h = Hashtbl.create 100 in
  let rec comb n k =
    match Hashtbl.find_opt h (n, k) with
    | Some x -> x
    | None ->
      let r = if k = 0 || k = n then 1 else comb (n - 1) (k - 1) + comb (n - 1) k in
      Hashtbl.add h (n, k) r;
      r
  in
  let ans = ref 0 in
  for k = 1 to 100 do
    let rec loop n =
      if n > 100
      then ()
      else if comb n k > 1_000_000
      then ans := !ans + 100 - n + 1
      else loop (n + 1)
    in
    loop (k + 1)
  done;
  !ans
;;

let run _ = solve () |> string_of_int
