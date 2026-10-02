let get_pyt m n = (m * m) - (n * n), 2 * m * n, (m * m) + (n * n)

let solve () =
  List.init 33 (fun i -> i + 1)
  |> List.concat_map (fun i ->
    List.init (33 - i) (fun j -> j + 1) |> List.map (get_pyt i))
  |> List.find (fun (a, b, c) -> a + b + c = 1000)
  |> fun (a, b, c) -> a * b * c
;;

let run _ = solve () |> string_of_int
