let piece = [|1;2;5;10;50;100;200|]



let f x =
  let t = Array.make x None in
  let rec aux t x = 
    if x <= 0 then 0 else
    let s = ref 0 in
   let k = ref 0 in
    while (!k)<Array.length piece && piece.(!k)<=x do
    let m = match t.(x-piece.(!k)) with
        |None -> let q = (if piece.(!k) = x then 1 else aux t (x-piece.(!k))) in t.(x-piece.(!k))<- Some q ; q
        |Some a -> a
    in
    s:=!s+m ;
    ; k:=!k+1
  done ; !s 
  in aux t x ;;

(* cohérent avec la version naive*)


(* le nombre est trop grand pour être représenté en caml*)