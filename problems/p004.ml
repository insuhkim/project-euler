let () =
  let is_palindrome n =
    let s = string_of_int n in
    let l = String.length s in
    String.init l (fun i -> s.[l - i - 1]) = s
  in
  let largest = ref 0 in
  for i = 999 downto 100 do
    for j = i downto 100 do
      let n = i * j in
      if n > !largest && is_palindrome n then largest := n
    done
  done;
  print_int !largest
;;
