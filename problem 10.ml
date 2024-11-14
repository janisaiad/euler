(*nombre trop grand pour ocaml mince*)


let est_premier p =
  let d = ref 2 in
  while (!d)*(!d)<=p && (p mod (!d) <>0 ) do
    d:=!d+1
  done ; (!d)*(!d)>p ;;



let sum_of_prime n = 
  let rec aux u s =
    if u = 0 then s else aux (u-1) (s+ (if est_premier u then u else 0))
  in aux n 0 ;;




(*version avec le crible d'eratosthene*)