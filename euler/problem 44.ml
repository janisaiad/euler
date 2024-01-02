


let penta n = n*(3*n-1)/2 ;;

(* on va procéder selon la somme croissante*)

let est_dedans

(*comment optimiser le parcours*)


let trouve n = (*le mode opératoire optimisé implique de ne pas trop aller trop loin car pour procéder par minimum il faudrait parcourir un nombre infini de termes dès le début*)
  let t = Array.init n penta
  
  
