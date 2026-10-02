let rec factorial = function
  | 0 -> 1
  | n -> n * factorial (n - 1)
;;

let digit_list_of_int n =
  n
  |> string_of_int
  |> String.to_seq
  |> List.of_seq
  |> List.map (fun c -> Char.code c - Char.code '0')
;;

let f n = digit_list_of_int n |> List.map factorial |> List.fold_left ( + ) 0

let solve () =
  let rec loop i acc =
    if i > 2_540_160 then acc else loop (i + 1) (if i = f i then i :: acc else acc)
  in
  loop 3 [] |> List.fold_left ( + ) 0
;;

let run _ = solve () |> string_of_int
