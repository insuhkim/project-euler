let () =
  let n = 20 in
  let rec gcd n m = if m mod n = 0 then n else gcd (m mod n) n in
  let lcm n m = n / gcd n m * m in
  List.init n (fun i -> i + 1) |> List.fold_left lcm 1 |> print_int
;;
