(*
p0^(2^d0 - 1) * ... * pk^(2^dk - 1)
sum di = 500_500
*)

let sieve n =
  let is_prime = Array.make (n + 1) true in
  is_prime.(0) <- false;
  is_prime.(1) <- false;
  for p = 2 to int_of_float (sqrt (float n)) do
    if is_prime.(p)
    then (
      let i = ref (p * p) in
      while !i <= n do
        is_prime.(!i) <- false;
        i := !i + p
      done)
  done;
  is_prime
;;

let primes n =
  let limit =
    let n = float n in
    n *. (log n +. log (log n)) |> int_of_float
  in
  let sieve = sieve limit in
  for i = limit downto 2 do
    if sieve.(i)
    then (
      let k = ref 2 in
      let rec ( ^ ) = fun n k -> if k = 0 then 1 else n * (n ^ (k - 1)) in
      while i ^ !k <= limit do
        sieve.(i ^ !k) <- true;
        k := 2 * !k
      done)
  done;
  Array.to_seq sieve
  |> Seq.mapi (fun i b -> if b then Some i else None)
  |> Seq.filter_map Fun.id
  |> Seq.take n
  |> Array.of_seq
;;

let solve n =
  let primes = primes n in
  let m = 500_500_507L in
  let ( *! ), ( %! ) = Int64.(mul, rem) in
  let ans = ref 1L in
  Array.iter (fun p -> ans := !ans *! Int64.of_int p %! m) primes;
  !ans
;;

let run _ =
  assert (solve 4 = 120L);
  solve 500_500 |> Int64.to_string
;;
