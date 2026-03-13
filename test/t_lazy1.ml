external print_int : int -> unit = "prim_print_int_clot"

let print_and_return =
  let print_and_return = fun i -> fun r -> let _ = print_int i in r in
  print_and_return

let out =
  let result = (print_and_return 1 true) || (print_and_return 2 false) in
  let result = (print_and_return 1 false) && (print_and_return 2 false) in
  0
