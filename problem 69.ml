let crible = [] ;;
(* on calculera le crible sur un million, puis on le foutra sur une liste*)


let rec facteurs_premiers n u= (* u est la liste des premiers déjà calculée, crible quoi*)
  let rec aux m p s = (*aux calcule la valuation*)
    if m mod p <> 0 then s else aux (m/p) p (s+1)
  in match u with
    |[] -> []
    |x::r -> if n mod x = 0 then let q = aux n x 0 in (x,q)::(facteurs_premiers (n/(power x q)) r) else facteurs_premiers n r ;;


let rec power a b = if b = 0 then 1 else a* (power a (b-1)) ;;


let totient n u=
  let m = facteurs_premiers n u in
  let rec aux p l = (* aux calcule le totient*)
  match l with
    |[] -> p
    |(a,b)::r  -> aux (p*(a-1)*(power a (b-1))) r
  in aux 1 m ;;


(* le minimum des totients est pour le dernier nombre premier plus petit que n *)

let liste t =
  let u = ref [] in
  let n = Array.length t in
  for k = n-1 downto 0 do
    match t.(k) with
    |None -> ()
    |Some x -> if x>=2 then
    u:= x::(!u)
  done ; !u ;;




let crible n =
  let t = Array.init (n+1) (fun i->Some i) in
  let maxi = int_of_float ( (float_of_int n)**0.5) +1 in 
  for k = 2 to maxi do
    let j = ref (2*k) in
    while (!j)<= n do
      t.(!j)<- None ; j:= !j+k
    done ;
  done ; liste t ;;


let max_totient n =
  let u = crible n in
  let m = ref min_float in
  let argu = ref 2 in
  for k = 2 to n do
    let q =( float_of_int (k) )/.(float_of_int (totient k u)) in
    if q>(!m) then begin
      argu := k ; m:= q
    end
  done ; (!m,!argu) ;;

(*calculer le totient par progrmamation dynamique sur les nombres premiers ?*)