let rec restitue u = match u with (*on suppose u écrit à l'envers*)
  |[] -> 0
  |x::r ->  x+10*(restitue r) ;;

let rec give n =
  let rec aux n =  if n = 0 then [] else (n mod 10)::( aux (n/10))
  in let a = aux n in (a,List.rev a) ;;

let ope n = let a,b = give n in restitue a + restitue b ;;

(*ok*)

let est_palindrome n =  let a,b = give n in a=b ;;

let est_lychrel n =
  let rec aux k cour = 
    if k = 1 then true else let u = ope cour in if est_palindrome u then false else aux (k-1) u
  in aux 50 n ;;

(*le code est bon, les nombres deviennent trop grands ..*)


let how_many n = (* on considère qu'après 50 itérations, il est de lychrel*)
    let rec aux u s = 
      if u = 0 then s else
      aux (u-1) (s+ if est_lychrel u then 1 else 0 ) 
    in aux n 0 ;;
