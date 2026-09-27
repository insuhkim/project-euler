let ( *! ), ( +! ), ( /! ), ( %! ) = Int64.(mul, add, div, rem)
let cache = Hashtbl.create 1_000_000
let collatz_next n = if n %! 2L = 0L then n /! 2L else (3L *! n) +! 1L

let rec collatz_length n =
  match Hashtbl.find_opt cache n with
  | Some length -> length
  | None ->
    let length = if n = 1L then 1 else collatz_length (collatz_next n) + 1 in
    Hashtbl.add cache n length;
    length
;;

let () =
  let answer = ref 1 in
  let longest = ref 1 in
  for n = 1 to 1_000_000 do
    let length = collatz_length (Int64.of_int n) in
    if length > !longest
    then (
      longest := length;
      answer := n)
  done;
  print_int !answer
;;
