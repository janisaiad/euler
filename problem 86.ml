let absF f = if f > 0.0 then f else (f *. -1.0);;

let est_entier x =
  absF (float_of_int (int_of_float x ) -. x)<=0.0000001 || absF (float_of_int (int_of_float x ) -. x-.1.0)<= 0.0000001 ;; (* on choisi une précision arbitraire*)

let evalue a b c x =
  let p,q,r = (float_of_int a,float_of_int b,float_of_int c)
  in (q*.q+.x*.x)**0.5+.(p*.p+.(r-.x)**2.)**0.5 ;;

let racine p q r = ((-.r*.q*.q-.p*.q*.r)/.(p*.p-.q*.q),(-.r*.q*.q+.p*.q*.r)/.(p*.p-.q*.q)) ;; (* fonction pour simplifier le codage hein*)

let choisi (a,b) = 
  if a>=0. then 
    if b>= a then a 
      else 
        if b >=0. then b
        else a
  else b ;;

let fonctionne a b c =
  let p,q,r = (float_of_int a,float_of_int b,float_of_int c) in
  let u=min (evalue a b c (choisi (racine p q r)))  (min (evalue c a b (choisi (racine r p q))) (evalue b c a (choisi (racine q r p))))
  in if est_entier u then true else false ;;

(* peut_etre stocker les couples dans une liste pour pouvoir l'étudier après*)
let nb_of_solutions_that_exceeds nb =
  let u = ref [] in
  let comp = ref 0 in
  let m = ref 1 in
  while !comp < nb do
    m:=!m+1 ;
    for k = 1 to !m do
      for j = 1 to !m do
        if fonctionne (!m) k j then (comp := !comp +1 ; u:=((!m,k,j))::(!u))
      done
    done
  done ; (!m,!u) ;; (* -1 car le tour de boucle en trop fera incrémenter m d'un au plus *)