let incr u = u:= !u +1 ;;


(* on peut calculer les entiers premiers en une fois et utiliser une hash table ou un tableau*)

let est_premier p =
  let d = ref 2 in
  while (!d)*(!d) <=p && p mod (!d) <> 0 do
    incr d 
  done ; (!d)*(!d)>p ;;

let fonctionne u =
  let k = ref 0 in
  while (!k)*(!k)<=(u/2) && not ( est_premier (u-2*(!k)*(!k)) ) do (*la condition d'arrêt implique que le k max donne un truc possiblement premier négatif, absurde en fait :) *)
    incr k
  done ; not ((!k)*(!k) >u/2 );;

let trouve = (*on se limite aux impairs*)
  let u = ref 1 in
  while fonctionne (!u) do
    incr u ;
    incr u
  done ; !u ;;

(*la réponse est 5777, validée par la street*)