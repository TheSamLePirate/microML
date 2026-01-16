open Ast

let tf x = Ty.Fixed.fix x ()

module Make(B:Bindings)(BT:FreshableBindings)(F:Fields) = struct
  module BMap = Map.Make(B)
  module BTMap = Map.Make(BT)

  open Expr

  type ty = (unit, F.t, BT.t, B.t) Ty.Fixed.t
  type typed_pattern = (ty, F.t, B.t) Fixed.u
  type typed_expr = (ty, F.t, B.t) Fixed.t
  type env = {
      fresh : BT.t;
      bindings : ty BMap.t;
      tybindings : ty BTMap.t
    }

  let ppty f ty = Ty.Fixed.pp F.pp BT.pp B.pp f ty

  let ppenv f env =
    Format.fprintf f "@[{fresh:%a;@\nbindings:%a;@\ntybindings:@[%a@]@\n}@]"
    BT.pp env.fresh
    (Lib.Pp.assoc B.pp ppty) (BMap.to_list env.bindings)
    (Lib.Pp.assoc BT.pp ppty) (BTMap.to_list env.tybindings)

  let empty = { fresh = BT.init; bindings = BMap.empty; tybindings = BTMap.empty }
  let fresh env v =
    let typevar =  Ty.Var env.fresh |> tf in
    {env with
      fresh=BT.next env.fresh;
      bindings = BMap.add v typevar env.bindings}, typevar
  let fresh' env =
    let typevar =  Ty.Var env.fresh |> tf in
    {env with fresh=BT.next env.fresh}, typevar

  let insert_mono env binding ty = { env with bindings = BMap.add binding ty env.bindings}
  let insert_poly env binding ty = { env with bindings = BMap.add binding ty env.bindings}

  let find_binding env binding = env, BMap.find binding env.bindings (* TODO généralisation *)

  let sort_sum li = List.sort (fun (a, _) (b, _) -> F.compare a b) li

  (* TODO utiliser le zip *)
  let rec unify env t1 t2 =
    Format.printf "env:%a@\nunify @[<v>-%a@\n-%a@]@\n" ppenv env ppty (tf t1) ppty (tf t2);
    match t1, t2 with
    | Ty.Var v1, Ty.Var v2 when v1 == v2 ->
       env, t1
    | Ty.Var v, t | t, Ty.Var v -> (* TODO *)
       let env, t = match BTMap.find_opt v env.tybindings with
       | None -> env, t
       | Some t2 -> unify env t (Ty.Fixed.unfix t2)
       in
       let env = {env with tybindings = BTMap.add v (tf t) env.tybindings } in
       env, t
    | Ty.Integer, Ty.Integer -> env, Ty.Integer
    | Ty.Char, Ty.Char -> env, Ty.Char
    | Ty.Bool, Ty.Bool -> env, Ty.Bool
    | Ty.Sum li1, Ty.Sum li2 ->
       let li1 = sort_sum li1 in
       let li2 = sort_sum li2 in
       let rec merge env acc li1 li2 = match li1, li2 with
         | [], li | li, [] -> env, List.append (List.rev acc) li
         | ((name1, ty1)::tl1), ((name2, ty2)::tl2) ->
            match F.compare name1 name2 with
              | -1 -> merge env ((name1, ty1)::acc) li2 tl1
              | 1 -> merge env ((name2, ty2)::acc) li1 tl2
              | 0 -> let env, ty = unify env (Ty.Fixed.unfix ty1) (Ty.Fixed.unfix ty2) in
                     merge env ((name1, (Ty.Fixed.fix ty ()))::acc) tl1 tl2
              | _ -> assert false
       in let env, li = merge env [] li1 li2
          in env, Ty.Sum li
    | Ty.Tuple li1, Ty.Tuple li2 ->
       begin match Lib.zip_opt li1 li2 with
       | None -> assert false (* TODO erreur de type *)
       | Some li ->
          let env, li = List.fold_left_map (fun env (t1, t2) -> unify env (Ty.Fixed.unfix t1) (Ty.Fixed.unfix t2)) env li in
          env, Ty.Tuple (List.map (fun t -> Ty.Fixed.fix t ()) li)
       end
    | Ty.Arrow (f1, t1), Ty.Arrow (f2, t2) ->
       let env, f1 = unify env (Ty.Fixed.unfix f1) (Ty.Fixed.unfix f2) in
       let env, t1 = unify env (Ty.Fixed.unfix t1) (Ty.Fixed.unfix t2) in
       env, Ty.Arrow(tf f1, tf t1)
    | _ -> assert false

  let merge_tysum env = function
    | hd::tl -> List.fold_left (fun (env, t1) t2 -> unify env t1 t2) (env, hd) tl
    | [] -> assert false

  let merge_tysum env li =
    let env, t = merge_tysum env (List.map Ty.Fixed.unfix li)
    in env, tf t

  let unify env ta tb =
    let env, t = unify env (Ty.Fixed.unfix ta) (Ty.Fixed.unfix tb)
    in env, tf t

  let ty_pattern _ pat env =
    match pat with
    | Pattern.Var v -> fresh env v
    | Tuple li -> let types = List.map (fun i -> Fixed.decoration_u i) li
                  in env, Ty.Tuple types |> tf
    | Construct (a, p) ->
       env, Ty.Sum [a, Fixed.decoration_u p] |> tf

  let apply_ty env (ty : ty) : ty = (* On applique la map de substitution *)
    Ty.Fixed.Deep.map
      (function
       | Var binding -> Ty.Fixed.unfix (BTMap.find binding env.tybindings) (* TODO récursif ? *)
      | x -> x )
      ty

  let ty_pattern pat env : env * typed_pattern = Fixed.Deep.decorate_foldmap_u ty_pattern pat env

  let ty_expr tra expr env =
    match expr with
    | Fixable.Integer i -> env, Ty.Integer |> tf, Fixable.Integer i
    | Fixable.Char c -> env, Ty.Char |> tf, Fixable.Char c
    | Fixable.Bool b -> env, Ty.Bool |> tf, Fixable.Bool b
    | Fixable.Tuple li ->
       let env, li = List.fold_left_map tra env li in
       let types = List.map Fixed.decoration li in
       env, Ty.Tuple types |> tf, Fixable.Tuple li
    | Fixable.Construct (a, e) ->
       let env, e = tra env e in
       let te = Fixed.decoration e in
       env, Ty.Sum [a, te] |> tf, Fixable.Construct (a, e)
    | Fixable.Lambda (c, a) ->
       let env, tc = fresh env c in
       let env = insert_mono env c tc in
       let env, a = tra env a in
       let ta = Fixed.decoration a in
       env, Ty.Arrow (tc, ta)|> tf, Fixable.Lambda (c, a)
    | Fixable.BinOp (a, op, b) ->
       let t0 = match op with
         | Add | Sub | Mul | Div | Lt | Le | Gt | Ge -> Some(tf Ty.Integer)
         | And | Or -> Some (tf Ty.Bool)
         | _ -> None
       in
       let env, a = tra env a in
       let env, b = tra env b in
       let ta = Fixed.decoration a in
       let tb = Fixed.decoration b in
       let env, t = unify env ta tb in
       let env, t = match t0 with
         | None -> env, t
         | Some t0 -> unify env t0 t in
       env, t, Fixable.BinOp (a, op, b)
    | Fixable.UnOp (op, a) ->
       let t = match op with
         | Neg -> Ty.Integer
         | Not -> Ty.Bool in
       let env, a = tra env a in
       let ta = Fixed.decoration a in
       let t = tf t in
       let env, t = unify env ta t in
       env, t, Fixable.UnOp(op, a)
    | Fixable.Apply (a, b) ->
       let env, a = tra env a in
       let ta = Fixed.decoration a in
       let env, b = tra env b in
       let tb = Fixed.decoration b in
       let env, tout = fresh' env in
       let t = Ty.Arrow (tb, tout)|> tf in
       let env, _t = unify env t ta in
       env, tout, Fixable.Apply (a, b)
    | Fixable.Record li ->
       let env, li = List.fold_left_map (fun env (a, b) ->
                         let env, b = tra env b in
                         env, (a, b)) env li in
       let t = Ty.Record (List.map (fun (a, b) -> a, Fixed.decoration b) li) |> tf in
       env, t, Record (li)
    | Fixable.PatternMatch (a, li) ->
       let env, a = tra env a in
       let ta = Fixed.decoration a in
       let env, li = List.fold_left_map (fun env (pat, e) ->
                         let env, p = ty_pattern pat env in
                         let env, e = tra env e in
                         env, (p, e)) env li in
       let patterns, expressions = List.split li in
       let env, ty_sum = merge_tysum env (List.map Fixed.decoration_u patterns) in
       let env, tout = match expressions with
         | [] -> assert false
         | hd::tl ->
            let tout = Fixed.decoration hd in
            List.fold_left (fun (env, tout) e -> unify env tout (Fixed.decoration e)) (env, tout) tl in
       let env, _ty_sum = unify env ty_sum ta in
       env, tout, Fixable.PatternMatch (a, li)
    | Fixable.AccessRecord (e, b) ->
       let env, e = tra env e in
       let te = Fixed.decoration e in
       let env, tfield = fresh' env in
       let t = Ty.Record [b, tfield] |> tf in
       let env, _t = unify env t te in
       env, tfield, Fixable.AccessRecord (e, b)
    | Fixable.IfThenElse (a, b, c) ->
       let env, a = tra env a in
       let env, b = tra env b in
       let env, c = tra env c in
       let ta = Fixed.decoration a in
       let tb = Fixed.decoration b in
       let tc = Fixed.decoration c in
       let env, tb = unify env tb tc in
       let env, _ta = unify env ta (tf Ty.Bool) in
       env, tb, Fixable.IfThenElse(a, b, c)
    | Fixable.LetIn (c, a, b) ->
       let env, a = tra env a in
       let ta = Fixed.decoration a in
       let env = insert_poly env c ta in
       let env, b = tra env b in
       let tb = Fixed.decoration b in
       env, tb, Fixable.LetIn(c, a, b)
    | Fixable.LetRecIn(c, a, b) ->
       let env, tc = fresh' env in
       let env = insert_mono env c tc in
       let env, a = tra env a in
       let ta = Fixed.decoration a in
       let env, _ta = unify env ta tc in
       let env, b = tra env b in
       let tb = Fixed.decoration b in
       env, tb, Fixable.LetRecIn(c, a, b)
    | Fixable.Var c ->
       let env, tc = find_binding env c in
       env, tc, Fixable.Var c

  let ty_expr fin _gin _tra (env:env) _d e = ty_expr (fun a b -> fin b a) e env

  let process e =
    let env = empty in
    let env, e = Expr.Fixed.Traverse.decorate_foldmap ty_expr (fun _ -> assert false) Lib.idacc Lib.idacc e env in
    (* TODO redécorate pour suivre la map.*)
    Format.printf "env=%a@\n" ppenv env;
    let app d _ = apply_ty env d in
    Expr.Fixed.Deep.decorate app app e

end
