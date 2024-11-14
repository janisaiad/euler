let valeur p q = p*q*(p+1)*(q+1)/4 ;;

let twosqrt_ n = int_of_float ( (2.*.float_of_int (n))**0.5) ;;

let borneinf n l = let m,ll=float_of_int n, float_of_int l in
  let alpha= (4.*.m/.(ll*.(ll+.1.)))
  in int_of_float (alpha**0.5) ;;


let compa n r m l ll stocke = 
  let q = abs (!r - n) in if q<(!m) then (m:=q ; stocke := (l,ll)) ;;


let area_of_the_nearest n = 
  let q = twosqrt_ n in 
  let m = ref max_int in
  let stocke = ref (0,0) in
  for l = 1 to q do
    let ll= ref (borneinf n l ) in
    let r = ref (valeur l (!ll) )in
    while !r<= n do
      compa n r m l (!ll) stocke ; ll:=!ll +1 ; r :=(valeur l (!ll))
    done ; compa n r m l (!ll) stocke (* on regarde le premier terme qui dépasse n, le minimum est nécéssairement les minimum des derniers plus petit ou premier plus grand *)
  
  done ; let a,b = !stocke in a*b ;;

(*parfaitement fonctionnel, 2772*)