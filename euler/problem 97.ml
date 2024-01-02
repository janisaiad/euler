let rec powermod a b n = if b = 0 then 1 else (a*(powermod a (b-1) n)) mod n ;;

let produitXpowerXsumXmod a b c d n (* a*b**c+d mod n*) =
  (a*(powermod b c n) + d) mod n ;;

(*problème résolu 10 milliard est trop grand, 8739992577 )