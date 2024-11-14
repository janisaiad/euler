let est_pal n =
  let rec liste n =
    let rec aux u n =
      if n = 0 then u
      else aux ((n mod 10)::u) (n/10)
    in aux [] n
  in let q = liste n in q = List.rev q ;;

let rec puissance n = 
  if n =0 then 1 else 10*(puissance (n-1)) ;;

(* on pourrait optimiser la recherche en trouvant où se trouvent les palidromes, peut-être une amélioration en log(n)*)
let largest_palindrome n (*n est le nombre de chiffres*) =
  let m = ref min_int in
  for k = puissance(n) to puissance(n+1)-1 do
    for j = puissance(n) to puissance(n+1)-1 do
      if est_pal (j*k) then
        m:=max (!m) (j*k)
    done ;
  done ; !m ;;

