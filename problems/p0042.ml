let t = List.init 100 (fun n -> n * (n + 1) / 2)

let is_triangle_word s =
  String.to_seq s
  |> Seq.map (fun c -> Char.code c - Char.code 'A' + 1)
  |> Seq.fold_left ( + ) 0
  |> fun n -> List.mem n t
;;

let solve words = List.map is_triangle_word words |> List.filter Fun.id |> List.length

let parse input =
  String.trim input |> String.replace_all ~sub:"\"" ~by:"" |> String.split_on_char ','
;;

let run input = input |> parse |> solve |> Int.to_string
