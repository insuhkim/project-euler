let get_number file =
  file
  |> Filename.chop_extension
  |> fun name -> String.sub name 1 (String.length name - 1) |> int_of_string
;;

let get_problems dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun file ->
    String.length file >= 5 && file.[0] = 'p' && Filename.check_suffix file ".ml")
  |> List.sort String.compare
;;

let make_dispatch problems =
  let cases =
    problems
    |> List.map (fun file ->
      let module_name = file |> Filename.chop_extension |> String.capitalize_ascii in
      Printf.sprintf "  | %d -> %s.run input\n" (get_number file) module_name)
    |> String.concat ""
  in
  Printf.sprintf
    "let run problem input =\n\
    \  match problem with\n\
     %s  | _ -> failwith \"unknown problem\"\n"
    cases
;;

let generate path =
  let contents = get_problems "problems" |> make_dispatch in
  Out_channel.with_open_text path (fun oc -> output_string oc contents)
;;

let () = generate Sys.argv.(1)
