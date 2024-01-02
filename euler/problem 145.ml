let rec digits_rev n = if n = 0 then [] else
  let q = n/10 in (n mod 10 )::(digits_rev q) ;;

let retablit u = 
  let rec aux v s = 
    match v with
    |[]-> s
    |x::r -> aux (r) (x+10*s)
  in aux u 0;;

let ope n = let u = digits_rev n in retablit u + n ;;


let rec que_impair u =
  match u with
    |[] -> true
    |x::r -> x mod 2 = 1 && que_impair r ;;


let is_reversible n = que_impair  (digits_rev (ope n)) ;;


(* on pourrait diviser le temps par 2 mais en vrai la mémorisation est trop énorme, ça ne vaut pas le coup*)
let how_many n =
  let s = ref 0 in
  for k = 0 to n do
    if is_reversible k then s:=!s+1
  done ; !s ;;