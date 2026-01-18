let out =
  let rec f n x =
    if n = 0 then x
    else f (n - 1) (Cons (n, x))
  in f 4 Nil
