let rec diffe n =
  let rec sum_squarred n =
    let rec aux1 a s=
    if a = 0 then s else aux1 (a-1) (s+a*a)
    in aux1 n 0 
  in
  let rec sum n = 
    let rec aux2 a s =
      if a=0 then s else aux2 (a-1) (s+a)
    in aux2 n 0
  in let q = sum n in -sum_squarred n + q*q ;;