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

let () = comb 40 20 |> print_int
