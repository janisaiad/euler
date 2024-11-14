let est_premier n =
  let rec aux n v =
    if n = 2 || v=1 then true
    else n mod v <> 0 && aux n (v-1) 
  in aux n (int_of_float (1.+.(float_of_int n)**0.5)) ;;

let nth n =
  let comp = ref 0 in
  let i = ref 2 in
  while !comp< n do
    if est_premier (!i) then comp := !comp +1 ; i:=!i+1
  done ; !i -1 ;;
