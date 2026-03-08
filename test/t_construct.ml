type ty = Foo of int | Bar of int

external print_int : int -> unit = "prim_print_int_clot"

let out =
  match Bar 2 with
  | Foo a -> print_int a
  | Bar a -> print_int (a + 10)
