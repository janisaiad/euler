let rec factorial n = if n = 0 then 1 else n*(factorial (n-1)) ;;

let sum_digit_factorial n =
  let rec aux u s =
    if u = 0 then s else
  let a = u mod 10 in aux (u/10) (s+ factorial a)
  in aux n 0 ;;


(*il suffit de vérifier jusqu'à 2696489*)

let sum_of_good_numbers n = (*n représente le max*) (*on ne compte pas 1 et 2*)
    let rec aux u s =
      if u = 2 then s else aux (u-1) (s+ (if sum_digit_factorial u = u then u else 0))
  in aux n 0 ;;

  (* la réponse est 40730, validé par la street**)