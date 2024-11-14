let rec sum n a b s = (* on fait une version récursive terminale*)
  if a> n then s else
  if a mod 2 = 0 then sum n (a+b) a (s+a)
  else sum n (a+b) a (s);;