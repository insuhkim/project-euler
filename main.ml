let find_input problem =
  let prefix = Printf.sprintf "p%04d_" problem in
  Sys.readdir "inputs"
  |> Array.to_list
  |> List.find_opt (String.starts_with ~prefix)
  |> Option.map (fun file -> "inputs/" ^ file)
;;

let () =
  let problem = int_of_string Sys.argv.(1) in
  let input =
    find_input problem
    |> Option.map (fun path -> In_channel.with_open_text path In_channel.input_all)
    |> Option.value ~default:""
  in
  Dispatch.run problem input |> print_endline
;;
