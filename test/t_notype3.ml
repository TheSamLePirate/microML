let a =
  let rec f = fun x ->
    if x then f 0 else f true
  in f
