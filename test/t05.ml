type 'a option = None | Some of 'a

external print_int : int -> unit = "prim_print_int_clot"

let out =
let optdefault x d = match x with
| Some x -> x
| None -> d
in
  let optmap f x = match x with
    | None -> None
    | Some x -> Some (f x)
  in print_int (optdefault (optmap (fun x -> x + 1) (Some 5)) 0)
