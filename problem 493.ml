(* on fait un tirage de nombre entre 0 et 6*)

let tirage taille n k = (*taille désigne la taille de l'urne, n désigne le nombre de tirages successifs, k le nombre de couleurs différentes, taille est un multiple de k*)
  let nb = taille/k (* nb est le nombre de boules par couleurs*) in
  let t = Array.make k 0 in
  let comp = ref 0 in
  while !comp <n do
    let a = Random.int k in
    if t.(a)<nb then begin 
      t.(a)<-t.(a)+1
    ; comp := !comp +1 end
  done ; t ;;


let nb_de_couleurs t = (*compte le nombre de couleurs différentes*)
  let comp = ref 0 in
  for k = 0 to Array.length t -1 do
    if t.(k)>0 then comp:=!comp+1
  done ; !comp ;;

let moyenne nb taille n k = (*calcule l'espérance*)
  let s = ref 0 in
  for j = 0 to nb-1 do
    let a = tirage taille n k in
    s:= !s + nb_de_couleurs a
  done ; (float_of_int (!s))/.(float_of_int nb) ;;


(* c'est la fin de l'étude probabiliste*)

(* début de la récursion naïve*)
(*écriture de n en j éléments tous inférieurs à maxi*)

let rec u n j maxi= 
  if n<=0 then 0 else
  if j=1 then if n<= maxi then 1 else 0 else
  if j<=0 then 0
  else let s = ref 0 in
  for i = 0 to maxi do
    s:=!s + u (n-i) (j-1) maxi
  done ; !s ;;


let esperance taille n k = (* k le nombre de couleurs différentes, taille le nombre de boules, on a autant de boules par couleurs donc on suppose taille divisible par k, n le nombre de tirages successifs*)
  let maxi = (taille/k) in (* c'est le nombre de boules par couleurs, dans l'exemple c'est 10*)
  let s = ref 0 in
  for j = 0 to k do
    s:=!s+ u n j maxi
  done ; 
  let ss= ref 0. in
  for j = 1 to k do
    ss:=!ss +. (float_of_int (j*(u n j maxi)))/.(float_of_int (!s))
  done ; !ss ;;


(* fin de la récursion naïve*)

(* début de la programmation dynamique*)

let place u t n j = t.(n).(j)<- Some u ;;

let rec u n j maxi t= 
  if n>= Array.length t then (print_int n; failwith "error" )else
  if j>= Array.length (t.(0)) then (print_int j ; failwith "errorj") else
  if j<=0 ||
  if n<=0 || 
  if j*maxi <n then (place 0 t n j; 0) else
  if j=1 then if n<= maxi then (place 1 t n j; 1) else ( place 0 t n j ; 0) else
  match t.(n).(j) with
    |Some x -> x
    |None ->
  begin let s = ref 0 in
  for i = 0 to maxi do
    s:=!s + u (n-i) (j-1) maxi t
  done ;place (!s) t n j ; !s end;;

let esperance taille n k=
  let t = Array.make_matrix (n+1) (k+1) None in
  let maxi = (taille/k) in (* c'est le nombre de boules par couleurs*)
  let s = ref 0 in
  for j = 0 to k do
    s:=!s+ u n j maxi t
  done ; 
  let ss= ref 0. in
  for j = 1 to k do
    ss:=!ss +. (float_of_int (j*(u n j maxi t)))/.(float_of_int (!s))
  done ; !ss ;;