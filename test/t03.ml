type 'a list = Nil | Cons of 'a * 'a list

let out =
  let rec map f x = match x with
    | Nil -> Nil
    | Cons (hd, tl) -> Cons (f hd, map f tl)
  in map (fun x -> x + 1) Cons(1, Cons(2, Cons(3, Nil)))
