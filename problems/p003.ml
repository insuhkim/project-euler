let () =
  let n = 600_851_475_143 in
  let largest_prime_factor n =
    let rec factor m k =
      if k * k > m then m else if m mod k = 0 then factor (m / k) k else factor m (k + 2)
    in
    factor n 3
  in
  largest_prime_factor n |> print_int
;;
