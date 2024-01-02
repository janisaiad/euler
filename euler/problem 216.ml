let dicho f y prec deb fin =
  let rec aux a b =
    if b-.a<prec then (a,b) (* on renvoie la borne inf et sup *) else
    let m = (a+.b)/.2. in
    let p,q = f a, f b in 
    if f m < y then if p<q then aux m b else aux a m
    else if p<q then aux a m else aux m b
  in aux deb fin ;;

let find_r n s a b prec deb fin= (* a et b sont les paramètres de la somme*)
  let m = float_of_int n
  in dicho (fun r->a*.(1.-.r**m)/.(1.-.r)-.b*.(1.-.(m+.1.)*.r**m +.m*.r**m+.1.)/.((1.-.r)**2.)) s prec deb fin ;;


(*malheurement, les nombres donnés sont trop grands pour ocaml...*)