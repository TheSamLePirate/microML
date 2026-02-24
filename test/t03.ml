external print_int : int -> unit = "prim_print_int_clot"

type 'a list = Nil | Cons of 'a * 'a list


let out =
  let rec map f x = match x with
    | Nil -> Nil
    | Cons (hd, tl) -> Cons (f hd, map f tl) in
  let rec iter f x = match x with
    | Nil -> 0
    | Cons (hd, tl) ->
       let _ = f hd
       in iter f tl
  in iter print_int (map (fun x -> x + 1) Cons(1, Cons(2, Cons(3, Nil))))
