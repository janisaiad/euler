(* on utilise le crible d'ératosthène*)
(* on compte le nombre total et pas le nombre de classes d'équivalences, ceci aurait été plus cool*)
let permutation u =
  let rec aux v x =
    match v with
    |[]->[x]
    |a::b -> a::(aux b x )
  in match u with
    |[] ->[]
    |x::r -> aux r x ;;
  
let digits x =
  let rec aux x =
    if x = 0 then []
    else let a = x/10 in (x mod 10)::(aux a)
  in List.rev ( aux x) ;;


let crible n =
  ;;

let howmany n =
  let t = crible n in
  for k = 0 to Array.length t -1 do
    if 

  
(* on va calculer au fur et à mesure, stocker les nombres circulaires avec une fonction digits-> int, dans une liste, on met true à chaque nombre dans un tableau, ce tableau va nous servir en regardant si il y a None (calculer) , false, ne rien faire , true aussi on compte les true*