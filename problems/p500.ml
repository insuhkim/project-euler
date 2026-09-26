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
    then
      for i = p * p to n do
        if i mod p = 0 then is_prime.(i) <- false
      done
  done;
  is_prime
;;

let primes n =
  let limit =
    let n = float n in
    n *. (log n +. log (log n)) |> int_of_float
  in
  let sieve = sieve limit in
  Array.to_seq sieve
  |> Seq.mapi (fun i b -> if b then Some i else None)
  |> Seq.filter_map Fun.id
  |> Array.of_seq
;;

module Q = Pqueue.MakeMinPoly (struct
    type 'a t = float * 'a

    let compare (p1, _) (p2, _) = Float.compare p1 p2
  end)

let solve n =
  let primes = primes n in
  let q = Q.of_array (Array.map (fun p -> log (float p), (p, 0)) primes) in
  for _ = 1 to n do
    let v, (p, d) = Option.value (Q.pop_min q) ~default:(log 2., (2, 0)) in
    Q.add q (v *. 2., (p, d + 1))
  done;
  let m = 500_500_507L in
  let ( *! ), ( %! ) = Int64.(mul, rem) in
  let rec modpow n k m = if k = 0 then 1L else n *! modpow n (k - 1) m %! m in
  Q.fold_unordered
    (fun acc (_, (p, d)) -> acc *! modpow (Int64.of_int p) ((1 lsl d) - 1) m %! m)
    1L
    q
;;

let () =
  assert (solve 4 = 120L);
  solve 500_500 |> Int64.to_string |> print_string
;;
