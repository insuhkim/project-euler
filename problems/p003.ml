let solve n =
  let largest_prime_factor n =
    let rec factor m k =
      if k * k > m then m else if m mod k = 0 then factor (m / k) k else factor m (k + 2)
    in
    factor n 3
  in
  largest_prime_factor n
;;

let run _ = solve 600_851_475_143 |> string_of_int
