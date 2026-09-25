let () =
  let limit = 4_000_000 in
  let rec loop (a, b) s =
    if b > limit then s else loop (b, a + b) (if b mod 2 = 0 then s + b else s)
  in
  loop (1, 2) 0 |> print_int
;;
