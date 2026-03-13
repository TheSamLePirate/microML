external print_int : int -> unit = "prim_print_int_clot"

let print_and_return = fun i -> fun r ->
  let _ = print_int i in r

let result = (print_and_return 1 true) || (print_and_return 2 false)
let result = (print_and_return 1 false) && (print_and_return 2 false)
