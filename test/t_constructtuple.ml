type ty = Foo of int * int

external print_int : int -> unit = "prim_print_int_clot"

let out =
  match Foo (1, 2) with
  | Foo (a, b) -> print_int b
