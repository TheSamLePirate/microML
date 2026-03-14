external print_int : int -> unit = "prim_print_int_clot"

type 'a mylist =
  Nil
  | Cons of 'a * 'a mylist

type 'a option =
  None
  | Some of 'a

let print_bool b = print_int (if b then 1 else 0)

let rec length xs =
  match xs with
  | Nil -> 0
  | Cons (_, tl) -> 1 + length tl

let rec map f xs =
  match xs with
  | Nil -> Nil
  | Cons (x, tl) -> Cons (f x, map f tl)

let rec fold_left f acc xs =
  match xs with
  | Nil -> acc
  | Cons (x, tl) -> fold_left f (f acc x) tl

let rec append a b =
  match a with
  | Nil -> b
  | Cons (x, tl) -> Cons (x, append tl b)

let rec filter p xs =
  match xs with
  | Nil -> Nil
  | Cons (x, tl) ->
      if p x then
        Cons (x, filter p tl)
      else
        filter p tl

let head_opt xs =
  match xs with
  | Nil -> None
  | Cons (x, _) -> Some x

let make_adder x =
  let add y = x + y in
  add

let compose f g =
  fun x -> f (g x)

let id = fun x -> x

let const x =
  fun _ -> x

let test_poly =
  let a = id 42 in
  let b = id true in
  let _ = print_int a in
  print_bool b

let test_closure =
  let add10 = make_adder 10 in
  let add3 = make_adder 3 in
  let _ = print_int (add10 5) in
  print_int (add3 5)

let test_compose =
  let add1 = fun x -> x + 1 in
  let double = fun x -> x * 2 in
  let h = compose add1 double in
  print_int (h 10)

let xs = Cons (1, Cons (2, Cons (3, Cons (4, Nil))))

let test_list_ops =
  let _ = print_int (length xs) in
  let ys = map (fun x -> x + 100) xs in
  let _ = print_int (length ys) in
  let zs = filter (fun x -> x = 2) xs in
  let _ = print_int (length zs) in
  let s = fold_left (fun acc x -> acc + x) 0 xs in
  print_int s

let test_append =
  let a = Cons (10, Cons (20, Nil)) in
  let b = Cons (30, Cons (40, Nil)) in
  let c = append a b in
  let _ = print_int (length c) in
  print_int (fold_left (fun acc x -> acc + x) 0 c)

let test_head_opt =
  match head_opt xs with
  | None -> print_int (-1)
  | Some x -> print_int x

let test_nested_match =
  let v = Some (Cons (7, Cons (8, Nil))) in
  match v with
  | None -> print_int (-10)
  | Some (Nil) -> print_int (-11)
  | Some (Cons (x, Nil)) -> print_int (x + 1000)
  | Some (Cons (x, Cons (y, _))) -> print_int (x + y)

let test_const =
  let f = const 99 in
  let _ = print_int (f true) in
  print_int (f 123)

let test_poly_list =
  let l1 = Cons (id 1, Cons (id 2, Nil)) in
  let l2 = Cons (id true, Cons (id false, Nil)) in
  let _ = print_int (length l1) in
  print_int (length l2)

let test_more_poly =
  let choose_first x y = x in
  let a = choose_first 12 true in
  let b = choose_first false 99 in
  let _ = print_int a in
  print_bool b
