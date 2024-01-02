(*on fait un parcours triangulaire parce que c'est plus rapide*)

(* trouve un triplet de pythagore dont la somme vaut n*)
(*prend 3 secondes pour n=1000, complexité cubique*)
(*200,375,425*)


let trouve n = 
  let u = ref [] in
  for k = 0 to n do
    for j = 0 to (k-1) do
      for l = 0 to (j-1)do
        if l+j+k=n && l*l+j*j=k*k
          then u:= ((l,j,k))::(!u)
      done
    done
  done ; !u ;;