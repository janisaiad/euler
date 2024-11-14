(* on stocke les partitions sous la forme de listes croissantes*)

let rec produit u = match u with
  |[]->1
  |x::r -> x* (produit r) ;;

let rec verif n u (* u va contenir la liste des partitions à k éléments*) = match u with
  |[] -> false
  |x::r -> n = produit x || verif n r ;;



let rec  full_1 n = if n = 0 then [] else 1::(full_1 (n-1)) ;;

let rec ajoute_1_partout u = match u with
  |[] -> []
  |x::r -> (x+1)::(ajoute_1_partout r) ;;


let ajoute_1_deb u = (1)::u ;;

let concat u v =
  let rec aux u v =
    match u with
      |[] -> v
      |x::r -> aux (r) (x::v)
  in aux (List.rev u) v ;;


 (* on renvoie la liste des partitions de n à k éléments, par essence, si on fait cet appel c'est que t.(n).(k) n'est pas calculé, donc non exhaustif *)
  
  


let rec  partitions n k t =
  if n<0 ||k<0 then [] else
  match t.(n).(k) with
  |Some u -> u
  |None -> 
  let a = begin 
  if n<k then [] else
  if n=k then [full_1 n]
  else if n = 0 || k = 0 then []
  else let u,v = partitions (n-1) (k-1) t, partitions (n-k) k t in
  concat (List.map ajoute_1_deb u ) (List.map ajoute_1_partout v )
  end 
  in t.(n).(k)<- Some a ; a  ;;



let fonctionne n k t =
  let a = partitions n k t in
  verif n a ;;

let trouve_tableau k t = (*on garde le tableau pour une meilleure complexité*)
  let a = ref k in (* a est plus grand que k, sinon on ne peut pas décomposer a en une somme de k éléments tous non nuls*)
  while not(fonctionne (!a) k t) do
    a:= !a +1
  done ; !a ;;

(*tout fonctionne parfaitement*)




let liste_l'ensemble_tableau kmax =
  let t = Array.make_matrix (3*kmax) (3*kmax) None in
  let rec aux k v =
    if k = 1 then v
    else aux (k-1) ((trouve_tableau k t)::v)
  in aux kmax [] ;;

(* on suppose la croissance*)
let reponse_au_probleme n =
  let rec somme_disjoint u = match u with 
  |[] -> 0
  |[x] -> x
  |x::y::r -> if x = y then somme_disjoint (y::r) else x+somme_disjoint (y::r)
  in somme_disjoint (liste_l'ensemble_tableau n) ;;
(* vérifié avec 12 et 61*)


let trouve k p = (*p est l'entier maximum auquel on mènera le calcul*)
  let t = Array.make_matrix (p+1) (p+1) None in
  let a = ref k in (* a est plus grand que k, sinon on ne peut pas décomposer a en une somme de k éléments tous non nuls*)
  while not(fonctionne (!a) k t) do
    a:= !a +1
  done ; (!a,t) ;;




let nb_de_partitions_calculees t =
  let n = Array.length t in
  let s = ref 0 in
  for k = 0 to n-1 do
    for j = 0 to n-1 do
      match t.(j).(k) with
        |None -> ()
        |Some x -> s:= !s+ List.length x 
    done;
    done ; !s ;;

(*ocaml n'a pas beaucoup de mémoire pour composer ..*)