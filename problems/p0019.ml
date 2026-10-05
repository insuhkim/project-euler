let solve () =
  let days_of_month is_leap = function
    | 2 when is_leap -> 29
    | 2 -> 28
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 4 | 6 | 9 | 11 -> 30
    | _ -> failwith "invalid month"
  in
  let is_leap_year y =
    if y mod 400 = 0 then true else if y mod 100 = 0 then false else y mod 4 = 0
  in
  let y = ref 1900
  and m = ref 1
  and diff = ref 0
  and ans = ref 0 in
  let next_month () =
    let is_leap = is_leap_year !y in
    diff := !diff + days_of_month is_leap !m;
    incr m;
    if !m > 12
    then (
      m := 1;
      incr y)
  in
  while !y < 1901 do
    next_month ()
  done;
  while !y <= 2000 do
    if !diff mod 7 = 6 then incr ans;
    next_month ()
  done;
  !ans
;;

let run _ = solve () |> Int.to_string
