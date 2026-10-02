let score s =
  let avalue c = int_of_char c - int_of_char 'A' + 1 in
  String.to_seq s |> Seq.map avalue |> Seq.fold_left ( + ) 0
;;

let solve sls =
  List.sort compare sls
  |> List.mapi (fun i s -> (i + 1) * score s)
  |> List.fold_left ( + ) 0
;;

let parse input =
  input
  |> String.trim
  |> String.split_on_char ','
  |> List.map (fun s -> String.sub s 1 (String.length s - 2))
;;

let run input = input |> parse |> solve |> string_of_int
