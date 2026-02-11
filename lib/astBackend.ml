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
    | Construct of int * 'var atom
    | MakeTuple of 'var atom list
    | Nth of int * 'var atom
    | MakeRecord of (int * 'var atom) list
    | Proj of int * 'var atom
    | CallDirect of 'fun_id * 'var atom list
    | CallClosure of 'var atom * 'var atom list
    | MakeClosure of 'fun_id * int * 'var atom list
  type ('e, 'fun_id, 'var) t =
    | Let of 'var * ('fun_id, 'var) rhs * 'e
    | LetIf of 'var * 'var atom * 'e * 'e * 'e
    | LetIfTag of 'var * 'var atom * int * 'e * 'e * 'e
    | Return of 'var atom
    | AssertFalse

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
  | Construct (i, e) -> Format.fprintf f "construct-%d %a" i (pp_atom ppvar) e
  | MakeTuple (li) -> Format.fprintf f "(%a)" (pp_list_atom ppvar) li
  | MakeRecord (li) -> Format.fprintf f "{%a}" (Pp.list
                                                  (Pp.sep "; ") (fun f (i, atom) ->
                                                    Format.fprintf f "%d = %a" i (pp_atom ppvar) atom)) li
  | Proj (i, a) -> Format.fprintf f "(%a).(%d)" (pp_atom ppvar) a i
  | Nth (i, a) -> Format.fprintf f "(%a).(%d)" (pp_atom ppvar) a i
  | CallDirect (fu, args) -> Format.fprintf f "call %a (%a)" ppfun fu (pp_list_atom ppvar) args
  | CallClosure (fu, args) -> Format.fprintf f "clot-call %a (%a)" (pp_atom ppvar) fu (pp_list_atom ppvar) args
  | MakeClosure (fu, i, li) -> Format.fprintf f "clot-make %a %d (%a)" ppfun fu i (pp_list_atom ppvar) li

  let pp ppa ppfun ppvar f = function
      | Let (v, rhs, e) -> Format.fprintf f "@[<v>let %a = %a in@\n%a@]" ppvar v (pp_rhs ppfun ppvar) rhs ppa e
      | LetIf (v, atom, e1, e2, e3) -> Format.fprintf f "let %a = if %a then@\n@[<v 2>  %a@]@\nelse@\n@[<v 2>  %a@] in@\n%a" ppvar v (pp_atom ppvar) atom ppa e1 ppa e2 ppa e3
      | LetIfTag (v, atom, i, e1, e2, e3) -> Format.fprintf f "let %a = if constructed-%d %a then@\n@[<v 2>  %a@]@\nelse@\n@[<v 2>  %a@] in@\n%a" ppvar v i (pp_atom ppvar) atom ppa e1 ppa e2 ppa e3
      | Return atom -> Format.fprintf f "return %a" (pp_atom ppvar) atom
      | AssertFalse -> Format.fprintf f "assert false"
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
      | Construct (i, e) -> ret (fun e -> Construct (i, e)) <*> foldmap_atom h e
      | MakeTuple li -> ret (fun li -> MakeTuple li) <*> fold_left_map (foldmap_atom h) li
      | Nth (i, a) -> ret (fun a -> Nth(i, a)) <*> foldmap_atom h a
      | MakeRecord li -> ret (fun li -> MakeRecord li) <*> fold_left_map_snd (foldmap_atom h) li
      | Proj (i, a) -> ret (fun a -> Proj(i, a)) <*> foldmap_atom h a
      | CallDirect (funid, li) -> ret (fun funid li -> CallDirect (funid, li)) <*> g funid <*> fold_left_map (foldmap_atom h) li
      | CallClosure (a, li) -> ret (fun a li -> CallClosure (a, li)) <*> foldmap_atom h a <*> fold_left_map (foldmap_atom h) li
      | MakeClosure (funid, i, li) -> ret (fun funid li -> MakeClosure (funid, i, li)) <*> g funid <*> fold_left_map (foldmap_atom h) li

    let foldmap f g h = function
      | Let (v, rhs, e) -> ret (fun v rhs e -> Let (v, rhs, e)) <*> h v <*> foldmap_rhs g h rhs <*> f e
      | LetIf (v, a, e1, e2, e3) -> ret (fun v a e1 e2 e3 -> LetIf (v, a, e1, e2, e3)) <*> h v <*> foldmap_atom h a <*> f e1 <*> f e2 <*> f e3
      | LetIfTag (v, a, i, e1, e2, e3) -> ret (fun v a e1 e2 e3 -> LetIfTag (v, a, i, e1, e2, e3)) <*> h v <*> foldmap_atom h a <*> f e1 <*> f e2 <*> f e3
      | Return a -> ret (fun a -> Return a) <*> foldmap_atom h a
      | AssertFalse -> ret AssertFalse
  end

end

module Fixed = Fix3(Fixable)

type ('deco, 'fun_id, 'var) func = {
  id : 'fun_id;
  args : 'var list;
  body : ('deco, 'fun_id, 'var) Fixed.t;
}

type ('deco, 'fun_id, 'var) program = {
  functions : ('deco, 'fun_id, 'var) func list;
  entry : ('var * ('deco, 'fun_id, 'var) Fixed.t) list;
}


module Make(B:Ast.Bindings)(BT:Ast.FreshableBindings) = struct

  type env = {
      fresh : BT.t;
      globalMap : BT.t B.Map.t;
      localMap : BT.t B.Map.t;
      arity :int  BT.Map.t
  }
  let empty = {fresh = BT.init; globalMap = B.Map.empty; localMap = B.Map.empty; arity = BT.Map.empty }

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

  let bind_global env binding =
    let binding' = env.fresh in
    let env = { env with fresh = BT.next env.fresh; globalMap = B.Map.add binding binding' env.globalMap}
    in env, binding'
  let bind_local env binding =
    let binding' = env.fresh in
    let env = { env with fresh = BT.next env.fresh; localMap = B.Map.add binding binding' env.localMap}
    in env, binding'

  let fresh env =
    let binding' = env.fresh in
    let env = { env with fresh = BT.next env.fresh; }
    in env, binding'

  let fix t = Fixed.fix t ()

  let makereturn make atom = make (fix (Return atom))

  type reduce_result = env * (unit, BT.t, BT.t) func list *
                    ( (unit, BT.t, BT.t) Fixed.t -> (unit, BT.t, BT.t) Fixed.t ) * BT.t Fixable.atom
  type reduce_val =
    | RedRes of reduce_result
    | RedLambda of BT.t list * (BT.t list -> reduce_result)

  let res a env = match a env with
    | RedRes (env, funs, make, atom) -> env, funs, make, atom
    | RedLambda (bindings, fun_) -> fun_ bindings

  (* TODO pattern matching optimisation *)
  let groupPatterns v_in li =
    let rec groups_fields groups = function
      | (Ast.Expr.Pattern.Construct (field, p), e)::tl ->
         groups_fields ((field, (p, e))::groups) tl
      | [(Ast.Expr.Pattern.Var v), e] -> groups, Some (v, e)
      | ((Ast.Expr.Pattern.Var _v), _e)::_tl -> assert false (* TODO message d'erreur *)
      | ((Ast.Expr.Pattern.Tuple _li), _e) :: _tl -> assert false (* TODO message d'erreur *)
      | [] -> groups, None
    in
    let groups_fields li =
      let groups, _else_opt = groups_fields [] li in
      let _groups = Lib.group_by groups
      in assert false
    in
    let rec groups_tuple patterns = function
      | [Ast.Expr.Pattern.Var v, e] -> patterns, Some (v, e)
      | ((Ast.Expr.Pattern.Var _v), _e)::_tl -> assert false (* TODO message d'erreur *)
      | (Ast.Expr.Pattern.Construct (_c, _p), _e) :: _tl -> assert false (* TODO *)
      | (Ast.Expr.Pattern.Tuple li, e) :: tl -> groups_tuple ((li, e)::patterns) tl
      | [] -> patterns, None
    in
    let groups_tuple patterns =
      let _groups, _else_opt = groups_tuple [] patterns
      in assert false
    in match li with
       | [Ast.Expr.Pattern.Var v, e] ->
          Fixable.Let (v, v_in, e)
       | ((Ast.Expr.Pattern.Tuple _li, _e) :: _tl) as li -> groups_tuple li
       | ((Ast.Expr.Pattern.Construct (_c, _p), _e) :: _tl) as li -> groups_fields li
       | (Ast.Expr.Pattern.Var _v, _e)::_tl -> assert false (** TODO message d'erreur : _tl ne sert à rien *)
       | [] -> assert false

  let reduce_expr used_vars expr env =
    match expr with
    | Ast.Expr.Fixable.Integer i -> RedRes(env, [], id, Fixable.Int i)
    | Ast.Expr.Fixable.Bool i -> RedRes(env, [], id, Fixable.Bool i)
    | Ast.Expr.Fixable.Char c -> RedRes(env, [], id, Fixable.Char c)
    | Ast.Expr.Fixable.Var v -> RedRes(env, [], id, b_to_atom v env)
    | Ast.Expr.Fixable.UnOp (op, a) ->
       let env, functionsa, makea, a' = res a env in
       let env, c = fresh env in
       RedRes(env, functionsa, makea << (fun in_ ->
         Fixable.Let (c, Fixable.Prim1(op, a'), in_) |> fix
       ), Fixable.Var c)
    | Ast.Expr.Fixable.BinOp (a, op, b) ->
       let env, functionsa, makea, a' = res a env in
       let env, functionsb, makeb, b' = res b env in (* TODO and or shortcut ? générer un if *)
       let env, c = fresh env in
       RedRes(env, List.append functionsa functionsb, makea << makeb << (fun in_ ->
         Fixable.Let (c, Fixable.Prim2(op, a', b'), in_) |> fix
       ), Fixable.Var c)
    | Ast.Expr.Fixable.Apply (a, b) ->
       let env, functionsa, makea, a' = res a env in
       let env, functionsb, makeb, b' = res b env in
       let env, c = fresh env in
       RedRes(env, List.append functionsa functionsb, makea << makeb << (fun in_ ->
         Fixable.Let (c, Fixable.CallClosure(a', [b']), in_) |> fix
       ), Fixable.Var c)
    | Ast.Expr.Fixable.IfThenElse (a, b, c) ->
       let env, functionsa, makea, a' = res a env in
       let env, functionsb, makeb, b' = res b env in
       let env, functionsc, makec, c' = res c env in
       let env, v = fresh env in
       RedRes(env, List.flatten [functionsa; functionsb; functionsc], makea << (fun in_ ->
         Fixable.LetIf (v, a', makereturn makeb b', makereturn makec c', in_) |> fix
       ), Fixable.Var v)
    | Ast.Expr.Fixable.LetIn (binding, a, b) (* TODO on va avoir un problème de binding ? *)
    | Ast.Expr.Fixable.LetRecIn (binding, a, b) ->
       let env, binding' = bind_local env binding in
       let env, functionsa, makea, a' = res a env in
       let env, functionsb, makeb, b' = res b env in
       RedRes(env, List.flatten [functionsa; functionsb], makea << (fun in_ ->
                Fixable.Let (binding', Atom a', makeb in_) |> fix), b')
    | Ast.Expr.Fixable.Lambda (binding, a) ->
       let env, binding' = bind_local env binding in
       begin match a env with
       | RedLambda (bindings, fun_) -> RedLambda (binding'::bindings, fun_)
       | RedRes (env, functionsa, makea, a' ) ->
       RedLambda ([binding'], (fun bindings ->
                    let env, funid = fresh env in
                    let clot_vars = List.filter_map (fun binding ->
                                        Option.bind
                                          (B.Map.find_opt binding env.localMap)
                                          (fun b -> if List.mem b bindings then None else Some b)
                                      )
                                      (B.Set.to_list used_vars) in
                    let clot_vars_atoms = List.map (fun v -> Fixable.Var v) clot_vars in
                    let env, v = fresh env in
                    let args = List.append clot_vars bindings in
                    let funs = {id=funid;
                                args=args;
                                body=makereturn makea a'} in
                    let arity = List.length args in
                    let env = {env with arity = BT.Map.add funid arity env.arity} in
                    env, List.flatten [functionsa; [funs]], (fun in_ ->
                      Fixable.Let (v, Fixable.MakeClosure (funid, arity, clot_vars_atoms), in_) |> fix), Fixable.Var v))
       end
    | Ast.Expr.Fixable.Tuple li ->
       let env, v = fresh env in
       let (env, functions, make), li = List.fold_left_map (fun (env, functions, make) e ->
                                            let env, functions', make', e = res e env in
                                            let functions = functions'::functions in
                                            (env, functions, make << make'), e
                                          ) (env, [], id) li in
       let functions = List.flatten (List.rev functions) in
       RedRes(env, functions, make << (fun _in -> Fixable.Let (v, Fixable.MakeTuple(li), _in) |> fix), Fixable.Var v)
    |Ast.Expr.Fixable.Construct (i, e) ->
       let env, v = fresh env in
       let env, functions, make, e = res e env in
       RedRes(env, functions, make << (fun _in -> Fixable.Let (v, Fixable.Construct(i, e), _in) |> fix), Fixable.Var v)
     |Ast.Expr.Fixable.Record li ->
       let env, v = fresh env in
       let (env, functions, make), li = List.fold_left_map (fun (env, functions, make) (i, e) ->
                                            let env, functions', make', e = res e env in
                                            let functions = functions'::functions in
                                            (env, functions, make << make'), (i, e)
                                          ) (env, [], id) li in
       let functions = List.flatten (List.rev functions) in
       RedRes(env, functions, make << (fun _in -> Fixable.Let (v, Fixable.MakeRecord(li), _in) |> fix), Fixable.Var v)
     |Ast.Expr.Fixable.AccessRecord (e, i) ->
       let env, v = fresh env in
       let env, functions, make, e = res e env in
       RedRes(env, functions, make << (fun _in -> Fixable.Let (v, Fixable.Proj(i, e), _in) |> fix), Fixable.Var v)
     |Ast.Expr.Fixable.PatternMatch (e, li) ->
       let env, functions, make, e = res e env in
       assert false

  let reduce_pat _used_vars pat = pat

(*  let reduce_pat used_vars pat = Ast.Expr.Fixed.fix_u pat used_vars *)

  (* On capture les variables de l'environement utilisées pour savoir lesquelles capturer *)
  let decorate_bindings_p _ p =
    let p = Ast.Expr.Fixed.SurfaceG.map Ast.Expr.Fixed.decoration_u id id p in
    match p with
    | Ast.Expr.Pattern.Var v -> B.Set.singleton v
    | x -> Ast.Expr.Fixed.SurfaceG.fold B.Set.union passacc passacc x B.Set.empty

  let decorate_bindings_e _ e =
    let e = Ast.Expr.Fixed.SurfaceF.map Ast.Expr.Fixed.decoration Ast.Expr.Fixed.decoration_u id id e in
    match e with
    | Ast.Expr.Fixable.Var v -> B.Set.singleton v
    | Ast.Expr.Fixable.LetIn (v, e1, e2) -> B.Set.union e1 e2 |> B.Set.remove v
    | Ast.Expr.Fixable.Lambda (v, e) -> B.Set.remove v e
    | Ast.Expr.Fixable.PatternMatch (a, li) -> B.Set.union a (List.fold_left (fun acc (p, e) -> B.Set.union acc (B.Set.diff e p)) B.Set.empty li)
    | x -> Ast.Expr.Fixed.SurfaceF.fold B.Set.union passacc passacc passacc x B.Set.empty

  let decorate_bindings e =
    Ast.Expr.Fixed.Deep.decorate
      decorate_bindings_e
      decorate_bindings_p e

  let process env expr =
    let expr = decorate_bindings expr in
    let env, functions, makee, atom = res (Ast.Expr.Fixed.Deep.reduce_d reduce_expr reduce_pat expr) env in
     env, functions, makereturn makee atom

  let process env toplvl =
    match toplvl with
    | Ast.TopLvl.TypeDecl (_tylist, _binding, _ty) -> env, {functions=[]; entry=[]}
    | Ast.TopLvl.Let (binding, expr)
    | Ast.TopLvl.LetRec (binding, expr) ->
       let args, body = unlambda expr in
       let env, binding' = bind_global env binding in
       let env, functions, body = process env body in
       match args with
       | [] -> env, {functions=functions; entry=[binding', body]}
       | li ->
          let env, li' = List.fold_left_map bind_local env li in
          let env = {env with arity = BT.Map.add binding' (List.length li') env.arity} in
          env, {functions=List.append functions [{
                                           id = binding';
                                           args = li';
                                           body = body
                                       }]; entry = []}

  let passe = {
      Ast.init = empty;
      process = process
  }

  let pp_fun f func = Format.fprintf f "function %a(%a){@\n@[<v2>  %a@]@\n}"
                        BT.pp func.id
                        (Lib.Pp.list (Lib.Pp.sep ", ") BT.pp) func.args
                        (Fixed.pp BT.pp BT.pp) func.body

  let debug f prog =
     Format.fprintf f "%a@\n%a@\n"
       (Lib.Pp.list (Lib.Pp.sep "@\n") pp_fun) prog.functions
       (Lib.Pp.list (Lib.Pp.sep "@\n") (fun f (var, body) -> Format.fprintf f "%a=@\n@[<v2>  %a@]" BT.pp var (Fixed.pp BT.pp BT.pp) body)) prog.entry

end
