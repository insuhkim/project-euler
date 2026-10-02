let comb n m =
  let memo = Hashtbl.create 100 in
  let rec f n m =
    if m = 0 || m = n
    then 1
    else (
      match Hashtbl.find_opt memo (n, m) with
      | Some x -> x
      | None ->
        let x = f (n - 1) (m - 1) + f (n - 1) m in
        Hashtbl.add memo (n, m) x;
        x)
  in
  f n m
;;

let solve n = comb (2 * n) n
let run _ = solve 20 |> string_of_int
