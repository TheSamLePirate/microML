open Lib

module Fixable = struct

  type 'var atom =
    | GlobalVar of 'var
    | Var of 'var
    | Int of int
    | Char of char
    | Bool of bool
    | Unit

  type ('fun_id, 'var) rhs =
    | Atom of 'var atom
    | Prim1 of Ast.Expr.unop * 'var atom
    | Prim2 of Ast.Expr.binop * 'var atom * 'var atom
    | MakeTuple of 'var atom list
    | Proj of int * 'var atom
    | CallDirect of 'fun_id * 'var atom list
    | CallClosure of 'var atom * 'var atom list
    | MakeClosure of 'fun_id * 'var atom list (* capture atoms -> env *)
  type ('e, 'fun_id, 'var) t =
    | Let of 'var * ('fun_id, 'var) rhs * 'e
    | LetIf of 'var * 'var atom * 'e * 'e
    | Return of 'var atom

let pp_atom ppvar f = function
  | GlobalVar v -> Format.fprintf f "global %a" ppvar v
  | Var v -> ppvar f v
  | Int i -> Format.fprintf f "%d" i
  | Char c -> Format.fprintf f "%C" c
  | Bool b -> Format.fprintf f "%b" b
  | Unit -> Format.fprintf f "()"

let pp_list_atom ppvar f li = Pp.list (fun f () -> Format.fprintf f ", ") (pp_atom ppvar) f li
let pp_rhs ppfun ppvar f = function
  | Atom v -> pp_atom ppvar f v
  | Prim2 (op, a1, a2) -> Format.fprintf f "%a %a %a" (pp_atom ppvar) a1 Ast.Expr.ppop op (pp_atom ppvar) a2
  | Prim1 (op, a) -> Format.fprintf f "%a %a"  Ast.Expr.ppunop op (pp_atom ppvar) a
  | MakeTuple (li) -> Format.fprintf f "(%a)" (pp_list_atom ppvar) li
  | Proj (i, a) -> Format.fprintf f "(%a).(%d)" (pp_atom ppvar) a i
  | CallDirect (fu, args) -> Format.fprintf f "call %a (%a)" ppfun fu (pp_list_atom ppvar) args
  | CallClosure (fu, args) -> Format.fprintf f "clot-call %a (%a)" (pp_atom ppvar) fu (pp_list_atom ppvar) args
  | MakeClosure (fu, li) -> Format.fprintf f "clot-make %a (%a)" ppfun fu (pp_list_atom ppvar) li

  let pp ppa ppfun ppvar f = function
      | Let (v, rhs, e) -> Format.fprintf f "@[<v 2>let %a = %a in@\n%a@]" ppvar v (pp_rhs ppfun ppvar) rhs ppa e
      | LetIf (v, atom, e1, e2) -> Format.fprintf f "let %a = if %a then@\n@[<v 2>  %a@]@\nelse@\n@[<v 2>  %a@]" ppvar v (pp_atom ppvar) atom ppa e1 ppa e2
      | Return atom -> Format.fprintf f "return %a" (pp_atom ppvar) atom

  module Make(F:Applicative) = struct
    open F
    include ListApp(F)
    let foldmap_atom h = function
      | GlobalVar v -> ret (fun v -> GlobalVar v) <*> h v
      | Var v -> ret (fun v -> Var v) <*> h v
      | Int i -> ret (Int i)
      | Bool b -> ret (Bool b)
      | Char c -> ret (Char c)
      | Unit -> ret Unit
    let foldmap_rhs g h = function
      | Atom v -> ret (fun v -> Atom v) <*> foldmap_atom h v
      | Prim2 (op, a1, a2) -> ret (fun a1 a2 -> Prim2 (op, a1, a2)) <*> foldmap_atom h a1 <*> foldmap_atom h a2
      | Prim1 (op, a) -> ret (fun a -> Prim1 (op, a)) <*> foldmap_atom h a
      | MakeTuple li -> ret (fun li -> MakeTuple li) <*> fold_left_map (foldmap_atom h) li
      | Proj (i, a) -> ret (fun a -> Proj(i, a)) <*> foldmap_atom h a
      | CallDirect (funid, li) -> ret (fun funid li -> CallDirect (funid, li)) <*> g funid <*> fold_left_map (foldmap_atom h) li
      | CallClosure (a, li) -> ret (fun a li -> CallClosure (a, li)) <*> foldmap_atom h a <*> fold_left_map (foldmap_atom h) li
      | MakeClosure (funid, li) -> ret (fun funid li -> MakeClosure (funid, li)) <*> g funid <*> fold_left_map (foldmap_atom h) li

    let foldmap f g h = function
      | Let (v, rhs, e) -> ret (fun v rhs e -> Let (v, rhs, e)) <*> h v <*> foldmap_rhs g h rhs <*> f e
      | LetIf (v, a, e1, e2) -> ret (fun v a e1 e2 -> LetIf (v, a, e1, e2)) <*> h v <*> foldmap_atom h a <*> f e1 <*> f e2
      | Return a -> ret (fun a -> Return a) <*> foldmap_atom h a
  end

end

module Fixed = Fix3(Fixable)

type ('fun_id, 'var) func = {
  id : 'fun_id;
  args : 'var list;
  body : (unit, 'fun_id, 'var) Fixed.t;
}

type ('fun_id, 'var) program = {
  functions : ('fun_id, 'var) func list;
  entry : (unit, 'fun_id, 'var) Fixed.t list;
}


module Make(B:Ast.Bindings)(F:Ast.Fields)(BT:Ast.FreshableBindings) = struct

  type env = {
      fresh : BT.t;
      globalMap : BT.t B.Map.t;
      localMap : BT.t B.Map.t;
  }
  let empty = {fresh = BT.init; globalMap = B.Map.empty; localMap = B.Map.empty }

  let b_to_atom b env =
    match B.Map.find_opt b env.localMap with
    | Some s -> Fixable.Var s
    | None -> Fixable.GlobalVar (B.Map.find b env.globalMap)

  let rec unlambda acc expr =
    match Ast.Expr.Fixed.unfix expr with
    | Ast.Expr.Fixable.Lambda (v, l) ->
       unlambda (v::acc) l
    | _ -> acc, expr

  let unlambda expr =
    let args, body = unlambda [] expr in
    List.rev args, body

  let bind env binding =
    let binding' = env.fresh in
    let env = { env with fresh = BT.next env.fresh; globalMap = B.Map.add binding binding' env. globalMap}
    in env, binding'

  let fresh env =
    let binding' = env.fresh in
    let env = { env with fresh = BT.next env.fresh; }
    in env, binding'

  let fix t = Fixed.fix t ()

  let reduce_expr expr env =
    match expr with
    | Ast.Expr.Fixable.Integer i -> env, [], id, Fixable.Int i
    | Ast.Expr.Fixable.Bool i -> env, [], id, Fixable.Bool i
    | Ast.Expr.Fixable.Char c -> env, [], id, Fixable.Char c
    | Ast.Expr.Fixable.Var v -> env, [], id, b_to_atom v env
    | Ast.Expr.Fixable.UnOp (op, a) ->
       let env, functionsa, makea, a' = a env in
       let env, c = fresh env in
       env, functionsa, makea << (fun in_ ->
         Fixable.Let (c, Fixable.Prim1(op, a'), in_) |> fix
       ), Fixable.Var c
    | Ast.Expr.Fixable.BinOp (a, op, b) ->
       let env, functionsa, makea, a' = a env in
       let env, functionsb, makeb, b' = b env in (* TODO and or shortcut ? générer un if *)
       let env, c = fresh env in
       env, List.append functionsa functionsb, makea << makeb << (fun in_ ->
         Fixable.Let (c, Fixable.Prim2(op, a', b'), in_) |> fix
       ), Fixable.Var c
    | Ast.Expr.Apply (a, b) ->

    | Ast.Expr.IfThenElse (c, a, b) ->
       let env, functionsa, makea, a' = a env in
       let env, functionsb, makeb, b' = b env in
       let env, functionsb, makec, c' = c env in
       let env, c = fresh env in
       env, List.flatten [functionsa; functionsb; functionsc], makea << (fun in_ ->
         Fixable.Let (c, Fixable.Prim2(op, a', b'), in_) |> fix
       ), Fixable.Var c
    | Ast.Expr.LetIn (c, a, b) ->
    | Ast.Expr.LetRecIn (c, a, b) ->


    | Ast.Expr.Lambda (c, a) ->
    | _ -> assert false
  let reduce_pat pat _env = match pat with
    | Ast.Expr.Pattern.Var _v -> assert false
    | Ast.Expr.Pattern.Tuple _li -> assert false
    | Ast.Expr.Pattern.Construct (_f, _p) -> assert false

  let process env expr =
    let env, functions, makee, atom = Ast.Expr.Fixed.Deep.reduce reduce_expr reduce_pat expr env in
     env, functions, makee (fix (Return atom))

  let process env toplvl =
    match toplvl with
    | Ast.TopLvl.TypeDecl (_tylist, _binding, _ty) -> env, {functions=[]; entry=[]}
    | Ast.TopLvl.Let (binding, expr)
    | Ast.TopLvl.LetRec (binding, expr) ->
       let args, body = unlambda expr in
       let env, binding' = bind env binding in
       let env, functions, body = process env body in
       match args with
       | [] -> env, {functions=functions; entry=[body]}
       | li ->
          let env, li' = List.fold_left_map bind env li in
          env, {functions=List.append functions [{
                                           id = binding';
                                           args = li';
                                           body = body
                                       }]; entry = []}

  let passe = {
      Ast.init = empty;
      process = process
  }

end
