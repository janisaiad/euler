let somme_carre n =
  let a = ref n in
  let s = ref 0 in
  while !a>0 do
      let q = !a mod 10 in
      s:= !s + q*q ; a:= (!a)/10
  done ; !s ;;

let rec fonctionne n = if n = 89 then true else
    if n = 1 then false else fonctionne (somme_carre n) ;;

let under_nb_qui_donnent_sur_89 nb = (*bon résultat trouvé, on ne compte pas 1 au final donc tout va bien*)
  let comp = ref 0 in
  for k = 1 to nb do
    if fonctionne k then comp := !comp +1
  done ; !comp ;; 