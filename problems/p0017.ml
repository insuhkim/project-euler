let rec eng_of_n = function
  | 0 -> ""
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | 8 -> "eight"
  | 9 -> "nine"
  | 10 -> "ten"
  | 11 -> "eleven"
  | 12 -> "twelve"
  | 13 -> "thirteen"
  | 14 -> "fourteen"
  | 15 -> "fifteen"
  | 16 -> "sixteen"
  | 17 -> "seventeen"
  | 18 -> "eighteen"
  | 19 -> "nineteen"
  | 20 -> "twenty"
  | 30 -> "thirty"
  | 40 -> "forty"
  | 50 -> "fifty"
  | 60 -> "sixty"
  | 70 -> "seventy"
  | 80 -> "eighty"
  | 90 -> "ninety"
  | 1000 -> "onethousand"
  | d ->
    let d2, d1, d0 = d / 100, d / 10 mod 10, d mod 10 in
    if d2 = 0
    then eng_of_n (d1 * 10) ^ eng_of_n d0
    else
      eng_of_n d2
      ^ "hundred"
      ^ (if d1 + d0 = 0 then "" else "and")
      ^ eng_of_n ((10 * d1) + d0)
;;

let solve n =
  List.init n succ
  |> List.map eng_of_n
  |> List.map String.length
  |> List.fold_left ( + ) 0
;;

let run _ = solve 1000 |> Int.to_string
