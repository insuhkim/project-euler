let usage () =
  Printf.printf
    "Usage: dune exec ./main.exe -- <problem>\n\
    \       dune exec ./main.exe -- --template <problem>\n\n\
     Options:\n\
    \  --template <problem>  Create a problem template\n"
;;

let problem_file problem = Printf.sprintf "problems/p%04d.ml" problem

let find_input problem =
  let prefix = Printf.sprintf "p%04d_" problem in
  Sys.readdir "inputs"
  |> Array.to_list
  |> List.find_opt (String.starts_with ~prefix)
  |> Option.map (fun file -> "inputs/" ^ file)
;;

let template = "let solve () = \"\"\nlet run _input : string = solve ()\n"

let create_template problem =
  let path = problem_file problem in
  if Sys.file_exists path
  then false
  else (
    Out_channel.with_open_text path (fun oc -> output_string oc template);
    Printf.printf "Created %s\n" path;
    true)
;;

let rebuild () =
  Printf.printf "Building...\n%!";
  match Sys.command "dune build" with
  | 0 -> true
  | _ ->
    Printf.eprintf "dune build failed.\n%!";
    false
;;

let rerun problem =
  let problem = string_of_int problem in
  Unix.execvp "dune" [| "dune"; "exec"; "./main.exe"; "--"; problem |]
;;

let run problem =
  let path = problem_file problem in
  if not (Sys.file_exists path)
  then (
    Printf.printf "Problem %d does not exist.\nCreate %s? [y/N] %!" problem path;
    match read_line () |> String.lowercase_ascii with
    | "y" | "yes" -> if create_template problem && rebuild () then rerun problem
    | _ -> ())
  else (
    let input =
      find_input problem
      |> Option.map (fun path -> In_channel.with_open_text path In_channel.input_all)
      |> Option.value ~default:""
    in
    Dispatch.run problem input |> print_endline)
;;

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> usage ()
  | [ _; ("--template" | "-t"); problem ] ->
    create_template (int_of_string problem) |> ignore
  | [ _; problem ] -> run (int_of_string problem)
  | _ -> usage ()
;;
