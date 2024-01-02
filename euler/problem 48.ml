(* on calcule modulo 1 milliard*)
(* 10 milliard est trop grand, c'est mort*)
let product_mod a b r = (* récursive terminale*)
  let rec aux a b r p =
    if b = 0 then p
    else aux a (b-1) r (p*a mod r)
  in aux a b r 1 ;;


let sum k r = (* on fait une version récursive terminale*)
  let rec aux s k r= 
    if k = 0 then s
    else aux (s+(product_mod k k r)) (k-1) r 
  in aux 0 k r;;