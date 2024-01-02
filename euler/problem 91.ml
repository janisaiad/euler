let est_triangle a b c = a*a+b*b=c*c ;;

let how_many_right n =
  let s = ref 0 in
  for x1= 1 to n do
    for x 2 = 1 to n do
      for y1 = 1 to n do
        for y2 = 1 to n do
          (*o)