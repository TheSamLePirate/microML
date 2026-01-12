open Ast

let tf x = Ty.Fixed.fix x ()

module Make(B:Bindings)(F:Fields) = struct
  module BMap = Map.Make(B)

  open Expr

  type ty = (unit, F.t, int) Ty.Fixed.t
  type typed_pattern = (ty, F.t, B.t) Fixed.u
  type typed_expr = (ty, F.t, B.t) Fixed.t
  type env = {
      fresh : int;
      bindings : ty BMap.t
    }

  let fresh env v =
    let typevar =  Ty.Var env.fresh |> tf in
    {fresh=env.fresh+1; bindings = BMap.add v typevar env.bindings}, typevar
  let fresh' env =
    let typevar =  Ty.Var env.fresh |> tf in
    {env with fresh=env.fresh+1}, typevar

  let unify env _ty1 _ty2 = env (* TODO *)

  let sort_sum li = List.sort (fun (a, _) (b, _) -> F.compare a b) li

  (* merge des patterns qui sortent d'un match *)
  let rec merge_tysum env t1 t2 =
    match t1, t2 with
    | Ty.Var v, t | t, Ty.Var v -> let env = unify env v t in env, t
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
              | 0 -> let env, ty = merge_tysum env (Ty.Fixed.unfix ty1) (Ty.Fixed.unfix ty2) in
                     merge env ((name1, (Ty.Fixed.fix ty ()))::acc) tl1 tl2
              | _ -> assert false
       in let env, li = merge env [] li1 li2
          in env, Ty.Sum li
    | Ty.Tuple li1, Ty.Tuple li2 ->
       begin match Lib.zip_opt li1 li2 with
       | None -> assert false (* TODO erreur de type *)
       | Some li ->
          let env, li = List.fold_left_map (fun env (t1, t2) -> merge_tysum env (Ty.Fixed.unfix t1) (Ty.Fixed.unfix t2)) env li in
          env, Ty.Tuple (List.map (fun t -> Ty.Fixed.fix t ()) li)
       end
    | Ty.Arrow _, _ -> assert false
    | _, Ty.Arrow _ -> assert false
    | _ -> assert false

  let merge_tysum env = function
    | hd::tl -> List.fold_left (fun (env, t1) t2 -> merge_tysum env t1 t2) (env, hd) tl
    | [] -> assert false

  let merge_tysum env li = merge_tysum env (List.map Ty.Fixed.unfix li)

  let ty_pattern _ pat env =
    match pat with
    | Pattern.Var v -> fresh env v
    | Tuple li -> let types = List.map (fun i -> Fixed.decoration_u i) li
                  in env, Ty.Tuple types |> tf
    | Construct (a, p) ->
       env, Ty.Sum [a, Fixed.decoration_u p] |> tf

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
       let env, a = tra env a in
       let ta = Fixed.decoration a in
       env, Ty.Arrow (tc, ta)|> tf, Fixable.Lambda (c, a)
    | Fixable.BinOp (a, op, b) ->
       let t = match op with
         | Add | Sub | Mul | Div -> Ty.Integer
         | And | Or -> Ty.Bool in
       let env, a = tra env a in
       let env, b = tra env b in
       let ta = Fixed.decoration a in
       let tb = Fixed.decoration b in
       let t = tf t in
       let env = unify env ta t in
       let env = unify env tb t in
       env, t, Fixable.BinOp (a, op, b)
    | Fixable.UnOp (op, a) ->
       let t = match op with
         | Neg -> Ty.Integer
         | Not -> Ty.Bool in
       let env, a = tra env a in
       let ta = Fixed.decoration a in
       let t = tf t in
       let env = unify env ta t in
       env, t, Fixable.UnOp(op, a)
    | Fixable.Apply (a, b) ->
       let env, a = tra env a in
       let ta = Fixed.decoration a in
       let env, b = tra env b in
       let tb = Fixed.decoration b in
       let env, tout = fresh' env in
       let t = Ty.Arrow (tb, tout)|> tf in
       let env = unify env t ta in
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
                         let env, e = tra env e in
                         let env, p = ty_pattern pat env in
                         env, (p, e)) env li in
       let patterns, expressions = List.split li in
       let env, ty_sum = merge_tysum env (List.map Fixed.decoration_u patterns) in
       let env, tout = fresh' env in
       let env = List.fold_left (fun env e -> unify env tout (Fixed.decoration e)) env expressions in
       let env = unify env ty_sum ta in
       env, tout, Fixable.PatternMatch (a, li)
    | Fixable.AccessRecord (e, b) ->
       let env, e = tra env e in
       let te = Fixed.decoration e |> Ty.Fixed.unfix in
       let env, tfield = fresh' env in
       let t = Ty.Record [b, tfield] |> tf in
       let env = unify env t te in
       env, tfield, Fixable.AccessRecord (e, b)
    | _ -> assert false
(*
    | Fixable.Var c ->
    | Fixable.LetIn (c, a, b) ->
    | Fixable.LetRecIn (c, a, b) ->
 *)
end
