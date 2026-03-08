external print_int : int -> unit = "prim_print_int_clot"
let out =
  match (2, 1) with
  | (a, b) -> print_int (a + b*10)
