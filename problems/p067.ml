let parse input : int list list =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map (fun s -> s |> String.split_on_char ' ' |> List.map int_of_string)
;;

let f dp l =
  if dp = []
  then l
  else (
    let rec adj = function
      | h1 :: h2 :: tl -> max h1 h2 :: adj (h2 :: tl)
      | hd -> hd
    in
    List.hd dp :: adj dp |> List.map2 ( + ) l)
;;

let solve ls = ls |> List.fold_left f [] |> List.fold_left max 0
let run input = input |> parse |> solve |> string_of_int
