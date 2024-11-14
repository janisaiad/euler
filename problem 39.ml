(*compte-t-on les permutations de côté adjacent*)
(*on cherche les solutions a<b<c*)

let marche a b c= a*a+b*b=c*c ;;

let nb_of_solutions p =
  let s = ref 0 in
  for a=1 to p -2 do
    for b = a+1 to p-2 do
      if marche a b (p-a-b) then s:=!s+1
      done ;
    done; !s ;;

let p_tel_que_maxdesol n = (*le perimetre qui donne le max et est inférieur ou égal à n*)
  let rec aux u p v = (*v stocke la valeur du nb de sol pour le périmètre p*)
    if u = 3 then p else
    let q = nb_of_solutions u in if q<=v then aux (u-1) p v else aux (u-1) u q 
  in aux n n min_int ;;


(*la réponse est 840*)