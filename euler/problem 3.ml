let est_premier n =
  let rec aux n v =
    if n = 2 || v=1 then true
    else n mod v <> 0 && aux n (v-1) 
  in aux n (int_of_float (1.+.(float_of_int n)**0.5)) ;;

let largest_prime_factor n =
    let rec aux q v = 
      if q = 1 then v else
      if est_premier q && n mod q = 0 then aux (q-1) (max v (max (n/q) q))
      else aux (q-1) v 
    in aux (int_of_float (1.+.(float_of_int n)**0.5)) 0 ;;


(* nombre trop grand ptdrrrr*)