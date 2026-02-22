external print_int : int -> unit = "prim_print_int_clot"
let out =
  let i = 1 in print_int (i + 1)
