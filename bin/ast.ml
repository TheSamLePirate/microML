open Lib

module Ty = struct
  type ('a, 'b, 'c) tofix =
    Integer
  | Char
  | Tuple of 'a list
  | Sum of ('b * 'a) list
  | Record of  ('b * 'a) list
  | Var of 'c

  module Fixed =
    Fix3(struct
        type ('a, 'b, 'c) t = ('a, 'b, 'c) tofix
        let pp ppa ppb ppc f = function
          | Integer -> Format.fprintf f "int"
          | Char -> Format.fprintf f "char"
          | Var x -> ppc f x
          | Tuple li -> Pp.list (fun f () -> Format.fprintf f " * ") ppa f li
          | Sum li -> Pp.list (fun f () -> Format.fprintf f "@\n| ") (fun f (b, a) -> Format.fprintf f "%a of %a" ppb b ppa a) f li
          | Record li ->
             Format.fprintf f "{ @[%a@]}"
             (Pp.list (fun f () -> Format.fprintf f "@\n; ") (fun f (b, a) -> Format.fprintf f "%a of %a" ppb b ppa a)) li

        module Make(F:Applicative) = struct
          open F
          include ListApp(F)
          let foldmap f g h = function
            | Integer -> ret Integer
            | Char -> ret Char
            | Tuple li -> ret (fun x -> Tuple x) <*> fold_left_map f li
            | Sum li -> ret (fun x -> Sum x) <*> fold_left_map_couple g f li
            | Record li -> ret (fun x -> Record x) <*> fold_left_map_couple g f li
            | Var x -> ret (fun x -> Var x) <*> h x
        end
      end)
  let fix = Fixed.fix
end

module Pattern = struct
  type ('a, 'b, 'c) tofix =
    | Var of 'c
    | Construct of 'b * 'a
  module Fixed =
    Fix3(struct
        type ('a, 'b, 'c) t = ('a, 'b, 'c) tofix
        let pp ppa ppb ppc f = function
          | Var v -> ppc f v
          | Construct (b, a) -> Format.fprintf f "%a(%a)" ppb b ppa a
        module Make(F:Applicative) = struct
          open F
             let foldmap f g h = function
               | Var c -> ret (fun x -> Var x) <*> h c
               | Construct (b, a) ->
                  ret (fun b a -> Construct (b, a)) <*> g b <*> f a
        end
      end)
  let fix = Fixed.fix

end

module Expr = struct

  type binop = | Add | Mul | Div
               | And | Or

  let ppop f = function
    | Add -> Format.fprintf f "+"
    | Mul -> Format.fprintf f "*"
    | Div -> Format.fprintf f "/"
    | And -> Format.fprintf f "and"
    | Or -> Format.fprintf f "or"

  type unop = Neg | Not

  let ppunop f = function
    | Neg -> Format.fprintf f "-"
    | Not -> Format.fprintf f "not"

  type ('a, 'b, 'c) tofix =
    | Integer of int
    | Bool of bool
    | Char of char
    | Tuple of 'a list
    | Untuple of 'c list * 'a * 'a
    | Construct of 'b * 'a
    | PatternMatch of 'a * ( ('b, 'c) Pattern.Fixed.t * 'a) list
    | Record of ('b * 'a) list
    | AccessRecord of 'a * 'b
    | Var of 'c
    | BinOp of 'a * binop * 'a
    | UnOp of unop * 'a
    | LetIn of 'c * 'a * 'a
    | LetRecIn of 'c * 'a * 'a
    | Lambda of 'c * 'a
    | Apply of 'a * 'a

  module Fixed =
    Fix3(struct
        type ('a, 'b, 'c) t = ('a, 'b, 'c) tofix
        let pp ppa ppb ppc f = function
          | Integer i -> Format.fprintf f "%d" i
          | Bool b -> Format.fprintf f "%b" b
          | Char c -> Format.fprintf f "%c" c
          | Tuple li -> Format.fprintf f "(@[%a@])" (Pp.list (fun f () -> Format.fprintf f ", ") ppa) li
          | Untuple (li, a, b) -> Format.fprintf f "let (%a) = %a@\nin @[%a@]" (Pp.list (fun f () -> Format.fprintf f ",") ppc) li ppa a ppa b
          | Construct (b, a) -> Format.fprintf f "%a(%a)" ppb b ppa a
          | PatternMatch (a, li) -> Format.fprintf f "match %a with@\n  @[%a@]" ppa a
                                      (Pp.list (fun f () -> Format.fprintf f "@\n")
                                         (fun f (p, e) -> Format.fprintf f "|%a -> %a" (Pattern.Fixed.pp ppb ppc) p ppa e )) li
          | Record li -> Format.fprintf f "{@[%a@]}" (Pp.list (fun f () -> Format.fprintf f ";@\n") (fun f (b, a) -> Format.fprintf f "%a=%a" ppb b ppa a)) li
          | AccessRecord (a, b) -> Format.fprintf f "(%a).%a" ppa a ppb b
          | Var c -> ppc f c
          | BinOp (a, op, b) -> Format.fprintf f "(%a %a %a)" ppa a ppop op ppa b
          | UnOp (op, a) -> Format.fprintf f "(%a %a)" ppunop op ppa a
          | LetIn (c, a, b) -> Format.fprintf f "let %a = %a@\nin@[ %a@]" ppc c ppa a ppa b
          | LetRecIn (c, a, b) -> Format.fprintf f "let rec %a = %a@\nin@[ %a@]" ppc c ppa a ppa b
          | Lambda (c, a) ->Format.fprintf f "(fun %a -> %a)" ppc c ppa a
          | Apply (a, b) -> Format.fprintf f "(%a) (%a)" ppa a ppa b

        module Make(F:Applicative) = struct
          open F
          include ListApp(F)
          module Pat = Pattern.Fixed.MakeApp(F)
          let foldmap f g h = function
            | (Integer _ | Bool _ | Char _) as x -> ret x
            | Tuple li -> ret (fun x -> Tuple x) <*> fold_left_map f li
            | Untuple (li, a, b) -> ret (fun li a b -> Untuple (li, a, b)) <*> fold_left_map h li <*> f a <*> f b
            | Construct (b, a) -> ret (fun b a -> Construct (b, a)) <*> g b <*> f a
            | PatternMatch (a, li) -> ret (fun a li -> PatternMatch (a, li)) <*> f a <*> fold_left_map (fun (p, a) -> ret (fun p a -> p, a) <*> Pat.foldmap g h p <*> f a ) li
            | Record li -> ret (fun li -> Record li) <*> fold_left_map (fun (b, a) -> ret (fun b a -> b, a) <*> g b <*> f a) li
            | AccessRecord (a, b) -> ret (fun a b -> AccessRecord (a, b)) <*> f a <*> g b
            | Var c -> ret (fun x -> Var x) <*> h c
            | BinOp (a, op, b) -> ret (fun a b -> BinOp(a, op, b)) <*> f a <*> f b
            | UnOp (op, a) -> ret (fun a -> UnOp (op, a)) <*> f a
            | LetIn (c, a, b) -> ret (fun c a b -> LetIn (c, a, b)) <*> h c <*> f a <*> f b
            | LetRecIn (c, a, b) -> ret (fun c a b -> LetRecIn (c, a, b)) <*> h c <*> f a <*> f b
            | Lambda (c, a) -> ret (fun c a -> Lambda (c, a)) <*> h c <*> f a
            | Apply (a, b) -> ret (fun a b -> Apply (a, b)) <*> f a <*> f b

        end
      end)
  let fix = Fixed.fix
end
