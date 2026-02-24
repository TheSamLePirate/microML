external print_int : int -> unit = "prim_print_int_clot"

let out =
  let apply a f = f a in print_int (apply 1 (fun x -> x + 1))
