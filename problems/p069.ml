let t n =
  let phi = Array.init (n + 1) Fun.id in
  for p = 2 to n do
    if phi.(p) = p
    then (
      let k = ref p in
      while !k < n do
        phi.(!k) <- phi.(!k) / p * (p - 1);
        k := !k + p
      done)
  done;
  phi
;;

let () =
  let n = 1_000_000 in
  let t = t n in
  Array.mapi (fun i k -> i, float_of_int i /. float_of_int k) t
  |> Array.fold_left
       (fun (maxi, maxv) (i, v) -> if v > maxv then i, v else maxi, maxv)
       (-1, -1.)
  |> fst
  |> print_int
;;
