

type exp = 
  |Vide
  |N of float
  |Plus of exp * exp
  |Moins of exp * exp
  |Fois of exp * exp
  |Div of exp * exp ;;

let rec evalue arbre = match arbre with
  
  |Vide -> 0.
  |N x -> x
  |Plus (a,b)-> evalue a +. evalue b
  |Moins (a,b) -> evalue a -. evalue b
  |Fois (a,b)-> evalue a *. evalue b
  |Div (a,b) -> let p,q= evalue a, evalue b in if q = 0.  then 0. else p /.q (* ça va poser problème*);;





let rec ajoute_pos_k u x k = (*k<len(u) nécéssairement*)
  if k = 0 then x::u else match u with
    |[] -> failwith "k trop grand"
    |m::r -> m::( ajoute_pos_k r x (k-1))
  ;;


let rec ajoute_pos_k_partout u x k =
  match u with
    |[] -> []
    |m::r -> (ajoute_pos_k m x k)::(ajoute_pos_k_partout r x k) ;;





let rec permutations set (* on va enlever le premier, prendre toutes les permutations du reste, puis ajouter à chacune ce que l'on a enlevé, à chaque position possible*) (* il y a des cas symétriques dans les évaluations et permutaitons couplées*) =
  match set with
    |[]-> [[]]
    |[x] -> [[x]]
    |x::r -> let m = permutations r in
    let rec aux k v =
      if k <0 then v else
      let q = ajoute_pos_k_partout m x k in
    aux (k-1) (q@ v)
    in aux (List.length r ) [] ;;








let construit x v = (* on a seulement 5 opérations de base*) (*a plus, b moins, c fois, d division 1, e division 2*)
  let u = ref [] in
  let a,b,c,d,e = ref v, ref v, ref v, ref v, ref v in
  while (!a)<>[] do
    match !a with
    |m::r -> 
    u:= (Plus (N x, m))::(Plus (m, N x))::(!u) ; a:= r
  done ;
  while (!b)<>[] do
    match !b with
    |m::r -> 
    u:= (Moins (N x, m))::(Moins ( m, N x))::(!u) ; b:= r
  done ;
  while (!c)<>[] do
    match !c with
    |m::r -> 
    u:= (Fois (N x, m))::(Fois (m, N x))::(!u) ; c:= r
  done ;
  while (!d)<>[] do
    match !d with
    |m::r -> 
    u:= (Div (m,N x))::(!u) ; d:= r
  done ;
  while (!e)<>[] do
    match !e with
    |m::r -> 
    u:= (Div (N x, m))::(!u) ; e:= r
  done ; !u ;; 

let arbres_equi u = match u with
  |a::z ->
  match z with
  |b::r ->
   match r with 
   |[x;y] -> 
   let q = [|Plus (N x, N y) ;  Fois (N x, N y);Moins (N x, N y) ;Div (N x, N y) ; Div (N y, N x)|] in
  let m = [|Plus (N a, N b) ;  Fois (N a, N b);Moins (N a, N b) ;Div (N a, N b) ; Div (N a, N b)|]
  in let tot = ref [] in
  for k = 0 to 4 do
    for j = 0 to 4 do
      tot:=(Div (q.(k),m.(j)))::(Div (m.(j),q.(k)))::(Fois (q.(k),m.(j)))::(Plus (q.(k),m.(j)))::( Moins (q.(k),m.(j)))::(!tot)
  done;
  done ; !tot ;;



let arbres_from_onepermut u (* u est la permutation*) (* on va fabriquer la forêt de tous les arbres possibles*) (* on stocke dans un array de liste d'arbres, t.(k) est l'ensemble des arbres avec k opérandes*) =  
  let rec aux u = match u with
  |[] -> failwith "vide"
  |[x] -> [N x]
  |[x; y] ->[Plus (N x, N y) ;  Fois (N x, N y);Moins (N x, N y) ;Div (N x, N y) ; Div (N y, N x)]
  |x::r ->  construit x (aux r) (* on parcourt v 5 fois pour fabriquer tous les arbres*)
  in  List.rev_append (arbres_equi u ) (aux u) ;;


let all_arbre set =
  let m = permutations set in
  let rec aux u v = match u with
    |[] -> v
    |x::r -> aux r (List.rev_append (arbres_from_onepermut x) v) 
  in aux m [] ;;


let absf x = if x<0. then -.x else x ;;

let rec sorted_positive u =
  let rec insertion x u = if x<1. || absf ((float_of_int (int_of_float x)) -.x)>0.00001 then u else match u with
    |[]-> [x]
    |a::b -> if a<x then a::( insertion x b)  else if a=x then insertion x b else x::a::b 
  in match u with
    |[]->[]
    |x::r ->insertion x (sorted_positive r) ;;


let intervalle_max set = 
  let a = all_arbre set in
  let u = sorted_positive (List.map evalue a) (* tri fusion maybe*)
  in let rec aux v = match v with
    |[] -> failwith "vide"
    |[x] -> x
    |x::y::r -> if y=x+.1. then aux (y::r) else (*donc y>x+1, x est donc le maximum atteint*) x
  in aux u ;;

let trouve =
  let m = ref min_float in
  let argu_set = ref [] (* c'est l'argument maximum*) in
  for a = 1 to 9 do
    for b = a+1 to 9 do
      for c = b+1 to 9 do
        for d = c+1 to 9 do
          let q = intervalle_max [float_of_int a;float_of_int b; float_of_int c;float_of_int d] in 
          if q>=(!m) then begin
            m:= q ;
            argu_set:= [a;b;c;d]::(!argu_set)
          end
        done ;
      done ;
    done; 
  done; (!argu_set,!m) ;;

(* j'avais une réponse juste, le code était certe incomplet mais ma réponse est juste, donc projet euler se fout de ma gueule*)


(*1258, c'est bon)