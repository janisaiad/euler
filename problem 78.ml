let partition n = 
  let t = Array.make_matrix (n+1) (n+1) None in
  let rec aux u v t = if u<0 || v<0 then 0 else
    match t.(u).(v) with
      |Some x -> x
      |None -> let a = (if u= 0 ||v = 0 || v>u then 0 else
      if u=v then 1 else let p,q = aux (u-1) (v-1) t , aux (u-v) (v) t in p+q)
       in t.(u).(v)<-Some (a) ; a
  in let s = ref 0 in
  for k = 1 to n do
    s:= !s + aux n k t
  done ; !s ;;




let partition_bis n t =
  let rec aux u v t = if u<0 || v<0 then 0 else
    match t.(u).(v) with
      |Some x -> x
      |None -> let a = (if u= 0 ||v = 0 || v>u then 0 else
      if u=v then 1 else let p,q = aux (u-1) (v-1) t , aux (u-v) (v) t in p+q)
       in t.(u).(v)<-Some (a) ; a
  in let s = ref 0 in
  for k = 1 to n do
    s:= !s + aux n k t
  done ; !s ;;

let who_is_the_first_integer_whose_the_partition_is_divisible_by_n n p = (*p désigne l'entier maximum dont on calculera la partition*)
    let k = ref 1 in
    let t = Array.make_matrix (p+1) (p+1) None in
    while !k<=p && partition_bis (!k) t mod n <> 0 do
      k:= !k+1
  done ; !k ;;

let a = who_is_the_first_integer_whose_the_partition_is_divisible_by_n 100 10000 ;;

(*caml limite tout ..*)