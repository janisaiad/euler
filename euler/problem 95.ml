let crible = [] ;;



let rec facteurs_premiers n u= (* u est la liste des premiers déjà calculée, crible quoi*)
  let rec aux m p s =
    if m mod p <> 0 then s else aux (m/p) p (s+1)
  in match u with
    |[] -> []
    |x::r -> if n mod x = 0 then let q = aux n x 0 in (q,x)::(facteurs_premiers (n/(power x q)) r) else facteurs_premiers n r ;;

let somme_diviseur n = let v = facteurs_premiers n crible in
    let rec aux l p =match l with
      |[]->p-n
      |(a,b)::r -> aux r ((1-power a (b+1))/(1-a))
    in aux v 0 ;;


let rec power a b = if b = 0 then 1 else a*(power a (b-1)) ;;

let chain n maxi =
  let rec aux u s mini =
    if u=0 || u=1  || u>maxi then (0,0) else
    if u = n then (s,mini)
    else aux (somme_diviseur u) (s+1) (min u mini)
  in aux n 0 n;;  