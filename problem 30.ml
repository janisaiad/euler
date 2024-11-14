(*validé,443839*)

let sum_fifth_power_digits n =
  let rec aux a s =
    if a = 0 then s else
    let q = a mod 10 in aux (a/10) (s+q*q*q*q*q)
  in aux n 0 ;;

let sum_of_all nb = 
  let rec aux n s =
    if n = 1 then s
    else if sum_fifth_power_digits n = n then aux (n-1) (s+n) else aux (n-1) s
  in aux nb 0 ;;