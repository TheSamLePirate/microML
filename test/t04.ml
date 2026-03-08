type 'a list = Nil | Cons of 'a * 'a list
external print_int : int -> unit = "prim_print_int_clot"

let out =
  let rec f n x =
    let _ = print_int n in
    if n = 0 then x
    else f (n - 1) (Cons (n, x))
  in f 4 Nil
