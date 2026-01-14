open Ast
open Lib

module Make(B:Bindings)(F:Fields) = struct

  module BindingsMap = Map.Make(B)
  module FieldsMap = Map.Make(F)

  type mlv =
    | Integer of int
    | Bool of bool
    | Char of char
    | Tuple of mlv list
    | Construct of F.t * mlv
    | Record of mlv FieldsMap.t
    | Lambda of (mlv -> mlv)
    | Cell of mlv option ref
  and env = mlv BindingsMap.t

  let rec eq a b = match a, b with
    | Integer a, Integer b -> a == b
    | Bool a, Bool b -> a == b
    | Char a, Char b -> a == b
    | Tuple l1, Tuple l2 -> Option.map (fun li -> List.fold_left (fun acc (a, b) -> acc && eq a b) true li) (Lib.zip_opt l1 l2) |> Option.value ~default:false
    | Construct (f1, e1), Construct (f2, e2) -> F.equal f1 f2 && eq e1 e2
    | Record l1, Record l2 ->
       Option.map (fun li -> List.fold_left (fun acc ((n1, e1), (n2, e2)) ->
                                 acc && F.equal n1 n2 && eq e1 e2
                               ) true li)
         (Lib.zip_opt (FieldsMap.to_list l1) (FieldsMap.to_list l2))
       |> Option.value ~default:false
    | _ -> false
  let rec pp f = function
  | Integer i -> Format.fprintf f "%d" i
  | Bool b -> Format.fprintf f "%b" b
  | Char c -> Format.fprintf f "%C" c
  | Tuple li -> Format.fprintf f "(%a)" (Lib.Pp.list (fun f () -> Format.fprintf f ",@ ") pp ) li
  | Construct (a, b) -> Format.fprintf f "%a %a" F.pp a pp b
  | Record li -> Format.fprintf f "{%a}" (Lib.Pp.list (fun f () -> Format.fprintf f ",@ ") (fun f (a, b) -> Format.fprintf f "%a=%a" F.pp a pp b)) (FieldsMap.to_list li)
  | Lambda _ -> Format.fprintf f "<fun>"
  | Cell r -> match !r with
              | Some r -> Format.fprintf f "Cell(%a)" pp r
              | None -> Format.fprintf f "Cell(None)"

let rec uncell f = function
  | Cell r -> uncell f (Option.get !r)
  | x -> f x

  let integer = uncell (function | Integer i -> i | x -> Pp.failwith "must be integer (%a)" pp x)
  let record = uncell (function | Record i -> i | x -> Pp.failwith "must be record (%a)" pp x)
  let bool = uncell (function | Bool i -> i | x -> Pp.failwith "must be bool (%a)" pp x)
  let lambda = uncell (function | Lambda i -> i | x -> Pp.failwith "must be lambda (%a)" pp x)
  let tuple = uncell (function | Tuple t -> t | x -> Pp.failwith "must be a tuple (%a)" pp x)

  let empty_env = BindingsMap.empty
  module E = Expr.Fixable

  let mapp = function
      | Expr.Pattern.Var v -> fun env e -> Some (BindingsMap.add v e env)
      | Expr.Pattern.Tuple li -> fun env e ->
                            List.fold_left (fun opt_env (pat, e) ->
                                Option.bind opt_env (fun env -> pat env e))
                              (Some env) (List.combine li (tuple e))
      | Expr.Pattern.Construct (field, p) ->
         fun env e ->
         match e with
         | Construct (field2, e) -> if F.equal field field2 then p env e else None
         | _ -> assert false

  let apply_patterns (a:mlv) (env:env) li : mlv =
    match List.fold_left (fun optenv (pat, e) -> match optenv with
                                            | None -> Option.map e (pat env a)
                                            | Some _ -> optenv) None li with
    | None -> failwith "bad pattern"
    | Some s -> s

  let mape = function
    | E.Integer i -> fun _ -> Integer i
    | E.Bool b -> fun _ -> Bool b
    | E.Char c -> fun _ -> Char c
    | E.Tuple li -> fun env -> Tuple (List.map (fun e -> e env) li)
    | E.Construct (b, a) -> fun env -> Construct (b, a env)
    | E.PatternMatch (a, li) ->
       fun env -> apply_patterns (a env) env li
    | E.Record li -> fun env -> Record (List.fold_left (fun map (field, a) -> FieldsMap.add field (a env) map) FieldsMap.empty li)
    | E.AccessRecord (a, f) -> fun env -> FieldsMap.find f (record (a env))
    | E.Var c -> BindingsMap.find c
    | E.BinOp (a, Expr.Add, b) -> fun env -> Integer (integer (a env) + integer (b env))
    | E.BinOp (a, Expr.Sub, b) -> fun env -> Integer (integer (a env) - integer (b env))
    | E.BinOp (a, Expr.Mul, b) -> fun env -> Integer (integer (a env) * integer (b env))
    | E.BinOp (a, Expr.Div, b) -> fun env -> Integer (integer (a env) / integer (b env))
    | E.BinOp (a, Expr.Gt, b) -> fun env -> Bool (integer (a env) > integer (b env))
    | E.BinOp (a, Expr.Ge, b) -> fun env -> Bool (integer (a env) >= integer (b env))
    | E.BinOp (a, Expr.Lt, b) -> fun env -> Bool (integer (a env) < integer (b env))
    | E.BinOp (a, Expr.Le, b) -> fun env -> Bool (integer (a env) <= integer (b env))
    | E.BinOp (a, Expr.And, b) -> fun env -> Bool (bool (a env) && bool (b env))
    | E.BinOp (a, Expr.Or, b) -> fun env -> Bool (bool (a env) || bool (b env))
    | E.BinOp (a, Expr.Eq, b) -> fun env -> Bool (eq (a env) (b env))
    | E.BinOp (a, Expr.Neq, b) -> fun env -> Bool (not (eq (a env) (b env)))
    | E.UnOp (Expr.Neg, a) -> fun env -> Integer (- integer (a env))
    | E.UnOp (Expr.Not, a) -> fun env -> Bool (not (bool (a env)))
    | E.LetIn (c, a, b) -> fun env -> b (BindingsMap.add c (a env) env)
    | E.Lambda (c, a) -> fun env -> Lambda ( fun b -> a (BindingsMap.add c b env))
    | E.IfThenElse (a, b, c) -> fun env -> if bool(a env) then b env else c env
    | E.Apply (a, b) -> fun env -> (lambda (a env)) (b env)
    | E.LetRecIn (c, a, b) -> fun env ->
                              let cell = ref None in
                              let env' = BindingsMap.add c (Cell cell) env in
                              let v = a env' in
                              (match v with
                               | Lambda _ -> cell := Some v
                               | _ -> failwith "let rec: RHS must be a function");
                              b env'


let compile e = Expr.Fixed.Deep.reduce mape mapp e

end
