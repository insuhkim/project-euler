let solve n =
  let cache = Hashtbl.create 1_000_000 in
  let collatz_next n = if n mod 2 = 0 then n / 2 else (3 * n) + 1 in
  let rec collatz_length n =
    match Hashtbl.find_opt cache n with
    | Some length -> length
    | None ->
      let length = if n = 1 then 1 else collatz_length (collatz_next n) + 1 in
      Hashtbl.add cache n length;
      length
  in
  let answer = ref 1 in
  let longest = ref 1 in
  for i = 1 to n do
    let length = collatz_length i in
    if length > !longest
    then (
      longest := length;
      answer := i)
  done;
  !answer
;;

let run _ = solve 1_000_000 |> string_of_int
