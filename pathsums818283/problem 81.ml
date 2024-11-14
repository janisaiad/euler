let next_matrix1 m t k n = (* on modifie t*)
    t.(k).(0)<- t.(k-1).(0)+m.(k).(0) ;
    t.(0).(k)<- t.(0).(k-1) ;
    for j = 1 to k -1 do
      t.(k-j).(j)<- m.(k-j).(j) + min t.(k-j).(j-1) t.(k-j-1).(j)
    done  ;;

let next_matrix2 m t k n =
  for j = k-(n-1) to n-1 do
    t.(k-j).(j)<- m.(j).(k-j) + (min t.(j-1).(k-j) t.(j).(k-j-1)) 
  done ;;
  

let path_sum_two_ways m = (*on fait deux boucles pour distinguer les cas*)
  let n = Array.length m -1  in
  let t = Array.make_matrix n n 0 in (* f max_int min_int*)
  t.(0).(0)<- m.(0).(0) ;
  for k = 1 to n-1 do
    next_matrix1 m t k n
  done ;
  for k = n to 2*(n-1) do
    next_matrix2 m t k n
  done ; t.(n-1).(n-1) ;;
