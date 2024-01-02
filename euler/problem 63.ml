let digits n =
  let rec aux u q =
    if q = 0 then u
    else let a = q/10 in aux ((q mod 10 )::u) a
  in List.rev (aux [] n) ;;

let rec power a b = if b = 0 then 1 else a*(power a (b-1)) ;;

let nb_digits n = List.length (digits n) ;;

let how_many (* on compte pour chaque n inférieur à 10*) =
  let s = ref 0 in
  for k  = 2 to 9 do
    let j = ref 1 in
    while power k (!j) >= power 10 (!j-1) do
      if nb_digits (power k (!j)) = !j then s:=!s+1 ;
      j:= !j+1
    done ;
    done ; !s +1;;
