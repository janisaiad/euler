let rec digits n = if n = 0 then [] else
  let q = n/10 in let r = n mod 10 in r::(digits q) ;;

let nb_digits n = List.length (digits n );;



let verify n = (* nombre de fractions qui vérifient la propriété*)
  let nb = ref 0 in
  let c = ref (3,2) in
  for k = 1 to n do
    let a,b = !c in if nb_digits a >nb_digits b then begin nb:=!nb+1 end; c:=(a+2*b,a+b)
  done ; !nb ;;

(* ocaml ne supporte pas les grands nombres, toutefois le code est juste et complet*)