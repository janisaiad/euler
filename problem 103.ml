let rec parties u k = if k>List.length u then [] else if k = 0 then [[]] else
  match u with
    |[] -> [[]]
    |x::r -> List.rev_append (ajoute_all x (parties r (k-1))) (parties r k) ;;

let rec ajoute_all x v = match v with
  |[] ->[]
  |m::r -> (x::m)::(ajoute_all x r) ;;


(* calculer par programmation dynamique ? impossible je pense)