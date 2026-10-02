let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let solve () =
  let count = Array.make 1001 0 in
  for m = 2 to 22 do
    for n = 1 to m - 1 do
      if gcd m n = 1 && (m + n) mod 2 = 1
      then (
        let p = 2 * m * (m + n) in
        let r = ref p in
        while !r <= 1000 do
          count.(!r) <- count.(!r) + 1;
          r := !r + p
        done)
    done
  done;
  count
  |> Array.mapi (fun n c -> n, c)
  |> Array.fold_left (fun (n', c') (n, c) -> if c' > c then n', c' else n, c) (0, 0)
  |> fst
;;

let run _ = solve () |> string_of_int
