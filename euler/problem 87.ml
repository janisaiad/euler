let est_premier u =
  let k = ref 2 in
  while (!k)*(!k)<u && (u) mod (!k) <> 0 do
    k:=!k+1
  done ;  (!k)*(!k)>u ;;


let liste t =
  let u = ref [] in
  for k=Array.length t -1 downto 0 do
    match t.(k) with
    |None -> ()
    |Some x ->u:= x::(!u)
  done ; !u ;;

let crible n =
  let t = Array.init n (fun i -> if i= 2 ||i=1||i=0 then None else Some i) in
  let k = ref 2 in
  while (!k)*(!k)<= n do
    let p = ref 2 in
    while (!p)*(!k)<= n -1 do
      t.((!p)*(!k))<- None ; p:=!p+1
    done ;k:=!k+1
  done ; 2::(liste t) ;;
(* parfaitement fonctionnel pour l'instant*)



let is_prime_square u = if u<=3 then false else 
  let a = int_of_float ((float_of_int u)**0.5) in ( est_premier a && a*a=u) ;;

let incr u = 
  match !u with 
  |[]-> max_int (* comme ça on arrête le processus*)
  |x::r -> u:= r ; x
  ;;


let fonctionne n u = (*u est la liste des premiers par ordre croissant*)
  let u1 = ref u (* on va stocker les nombres premiers on encore explorés*) in
  let p = ref (incr u1) in 
  let trouve = ref true in 
  while abs ((!p)*(!p)*(!p)*(!p))<=n && (!trouve) do
    let u2 = ref u (* on va stocker les nombres premiers on encore explorés*) in
    let q = ref (incr u2) in
    while abs ( (!q)*(!q)*(!q))<=n-(!p)*(!p)*(!p)*(!p) && (!trouve) do
      if is_prime_square ( n-(!p)*(!p)*(!p)*(!p)-(!q)*(!q)*(!q) )
      then trouve:= false
    ; q:= incr u2
    done
    ; p := incr u1
  done
  ; not(!trouve) ;;



let how_many n = 
  let t = crible (let a = float_of_int n in let q = a**(1./.3.) in int_of_float q +1) in
  let rec aux u s = 
    if u = 1 then s else
    aux (u-1) (s+ if fonctionne u t then 1 else 0)
  in aux n 0 ;;

let crible2 n =
  let rec aux u nb =
    if nb = 1 then u
    else aux (if est_premier nb then nb::u else u) (nb-1)
  in aux [] n ;;