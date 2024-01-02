(*validé, 76576500*)



let nb_of_divisors n = (*on peut la faire en racine de n sans récursif*) 
  let j = ref 1 in
  let comp = ref 0 in
  while (!j)*(!j)<n do
   if n mod (!j) = 0 then comp := !comp +2 ; j:= !j +1
  done ; if (!j)*(!j)=n then !comp +1 else !comp;; (* dernier test pour si c'est une racine carré*)


let highly_divisible_triangular_number nb =
  let courant = ref 1 in
  let indice = ref 2 in (*va stocker ce que l'on va rajouter si on passe la boucle*)
  while nb_of_divisors (!courant)<= nb do
    courant :=!courant+ !indice ; indice:=!indice+1
  done ; (!courant,!indice-1,nb_of_divisors(!courant)) ;; (*on rend aussi sa position dans la liste des nombres triangulaires*)
