let rec sum n =
  if n=0 then 0 else
  if (n mod 5 = 0 || n mod 3 =0) then n + sum (n-1)
  else sum (n-1);;