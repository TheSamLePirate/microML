open Lib

module Loc = struct
  type t = { startp : Lexing.position; endp : Lexing.position }
  let pp f l =
    let sp = l.startp and ep = l.endp in
    Format.fprintf f "%d:%d-%d:%d"
      sp.pos_lnum (sp.pos_cnum - sp.pos_bol)
      ep.pos_lnum (ep.pos_cnum - ep.pos_bol)

end

module Ty = struct
  type ('a, 'b, 'c) tofix =
    Integer | Char | Bool
  | Tuple of 'a list
  | Sum of ('b * 'a) list
  | Record of  ('b * 'a) list
  | Var of 'c
  | Arrow of 'a * 'a

  module Base = struct
        type ('a, 'b, 'c) t = ('a, 'b, 'c) tofix
        let pp ppa ppb ppc f = function
          | Integer -> Format.fprintf f "int"
          | Bool -> Format.fprintf f "bool"
          | Char -> Format.fprintf f "char"
          | Var x -> ppc f x
          | Tuple li -> Pp.list (fun f () -> Format.fprintf f " * ") ppa f li
          | Sum li -> Pp.list (fun f () -> Format.fprintf f "@\n| ") (fun f (b, a) -> Format.fprintf f "%a of %a" ppb b ppa a) f li
          | Record li ->
             Format.fprintf f "{ @[%a@]}"
             (Pp.list (fun f () -> Format.fprintf f "@\n; ") (fun f (b, a) -> Format.fprintf f "%a of %a" ppb b ppa a)) li
          | Arrow (a, b) -> Format.fprintf f "%a -> %a" ppa a ppa b

        module Make(F:Applicative) = struct
          open F
          include ListApp(F)
          let foldmap f g h = function
            | Integer -> ret Integer
            | Bool -> ret Bool
            | Char -> ret Char
            | Tuple li -> ret (fun x -> Tuple x) <*> fold_left_map f li
            | Sum li -> ret (fun x -> Sum x) <*> fold_left_map_couple g f li
            | Record li -> ret (fun x -> Record x) <*> fold_left_map_couple g f li
            | Var x -> ret (fun x -> Var x) <*> h x
            | Arrow (a, b) -> ret (fun a b -> Arrow (a, b)) <*> f a <*> f b
        end
        let align a b = match a, b with
            | Integer, Integer -> Some (Integer)
            | Char, Char -> Some (Char)
            | Bool, Bool -> Some (Char)
            | Tuple lia, Tuple lib -> Option.map (fun x -> Tuple x) (zip_opt lia lib)
            | Sum lia, Sum lib -> Option.map (fun x -> Sum x) (zip_opt lia lib)
            | Record lia, Record lib -> Option.map (fun x -> Record x) (zip_opt lia lib)
            | Var a, Var b -> Some (Var (a, b))
            | Arrow (a, b), Arrow (c, d) -> Some (Arrow ((a, c), (b, d)))
            | _ -> None
  end
  module Fixed = Fix3(Base)

  let chars = [| "α"; "β"; "γ"; "δ"; "ε"; "θ"; "φ"; "ψ"; "ω"; "Ω" |]
  let lenchars = Array.length chars
  let string_of_v i =
    let ci = i mod lenchars in
    let ni = i / lenchars in
    let str = chars.(ci) in
    if ni = 0 then "'" ^ str
    else "'" ^ str ^ (string_of_int ni)
end

module Expr = struct

  module Pattern = struct
    type ('a, 'b, 'c) t = (* TODO ajouter des constantes dedans *)
      | Var of 'c
      | Tuple of 'a list
      | Construct of 'b * 'a (* TODO AS...*)
    let pp ppa ppb ppc f = function
      | Var v -> ppc f v
      | Tuple li -> Format.fprintf f "(%a)" (Pp.list (fun f () -> Format.fprintf f ", ") ppa) li
      | Construct (b, a) -> Format.fprintf f "%a(%a)" ppb b ppa a
    module Make(F:Applicative) = struct
      include ListApp(F)
      open F
      let foldmap f g h = function
        | Var c -> ret (fun x -> Var x) <*> h c
        | Tuple li -> ret (fun x -> Tuple x) <*> fold_left_map f li
        | Construct (b, a) ->
           ret (fun b a -> Construct (b, a)) <*> g b <*> f a
    end
  end

  type binop = | Add | Sub | Mul | Div
               | Eq
               | Lt | Le | Gt | Ge | Neq
               | And | Or

  let ppop f = function
    | Add -> Format.fprintf f "+"
    | Sub -> Format.fprintf f "-"
    | Mul -> Format.fprintf f "*"
    | Div -> Format.fprintf f "/"
    | And -> Format.fprintf f "and"
    | Or -> Format.fprintf f "or"
    | Eq -> Format.fprintf f "=="
    | Neq -> Format.fprintf f "!="
    | Lt -> Format.fprintf f "<"
    | Gt -> Format.fprintf f ">"
    | Ge -> Format.fprintf f ">="
    | Le -> Format.fprintf f "<="

  type unop = Neg | Not

  let ppunop f = function
    | Neg -> Format.fprintf f "-"
    | Not -> Format.fprintf f "not"

  module Fixable = struct
    type ('a, 'p, 'b, 'c) t =
      | Integer of int
      | Bool of bool
      | Char of char
      | Tuple of 'a list
      | Construct of 'b * 'a
      | PatternMatch of 'a * ( 'p * 'a) list
      | Record of ('b * 'a) list
      | AccessRecord of 'a * 'b
      | Var of 'c
      | BinOp of 'a * binop * 'a
      | UnOp of unop * 'a
      | LetIn of 'c * 'a * 'a
      | LetRecIn of 'c * 'a * 'a
      | Lambda of 'c * 'a
      | Apply of 'a * 'a
    let pp ppa ppp ppb ppc f = function
      | Integer i -> Format.fprintf f "%d" i
      | Bool b -> Format.fprintf f "%b" b
      | Char c -> Format.fprintf f "%c" c
      | Tuple li -> Format.fprintf f "(@[%a@])" (Pp.list (fun f () -> Format.fprintf f ", ") ppa) li
      | Construct (b, a) -> Format.fprintf f "%a(%a)" ppb b ppa a
      | PatternMatch (a, li) -> Format.fprintf f "match %a with@\n  @[%a@]" ppa a
                                  (Pp.list (fun f () -> Format.fprintf f "@\n")
                                     (fun f (p, e) -> Format.fprintf f "|%a -> %a" ppp p ppa e )) li
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
      let foldmap f fp g h = function
        | (Integer _ | Bool _ | Char _) as x -> ret x
        | Tuple li -> ret (fun x -> Tuple x) <*> fold_left_map f li
        | Construct (b, a) -> ret (fun b a -> Construct (b, a)) <*> g b <*> f a
        | PatternMatch (a, li) -> ret (fun a li -> PatternMatch (a, li)) <*> f a <*> fold_left_map (fun (p, a) -> ret (fun p a -> p, a) <*> fp p <*> f a ) li
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
  end
  module Fixed = FixMut(Fixable)(Pattern)
end


module type Bindings = sig
  include Map.OrderedType
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end
module type Fields = sig
  include Map.OrderedType
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

