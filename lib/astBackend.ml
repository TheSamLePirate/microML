open Lib

module Fixable = struct

  type 'var atom =
    | GlobalVar of 'var
    | Native of string
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
    | Destruct of int * 'var atom
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
  | Native v -> Format.fprintf f "native %s" v
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
  | Destruct (i, e) -> Format.fprintf f "destruct-%d %a" i (pp_atom ppvar) e
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
      | Native v -> ret (Native v)
      | GlobalVar v -> ret (fun v -> GlobalVar v) <*> h v
      | Var v -> ret (fun v -> Var v) <*> h v
      | Int i -> ret (Int i)
      | Bool b -> ret (Bool b)
      | Char c -> ret (Char c)
      | Unit -> ret Unit
    let foldmap_rhs foldmap_atom g h = function
      | Atom v -> ret (fun v -> Atom v) <*> foldmap_atom h v
      | Prim2 (op, a1, a2) -> ret (fun a1 a2 -> Prim2 (op, a1, a2)) <*> foldmap_atom h a1 <*> foldmap_atom h a2
      | Prim1 (op, a) -> ret (fun a -> Prim1 (op, a)) <*> foldmap_atom h a
      | Construct (i, e) -> ret (fun e -> Construct (i, e)) <*> foldmap_atom h e
      | Destruct (i, e) -> ret (fun e -> Destruct (i, e)) <*> foldmap_atom h e
      | MakeTuple li -> ret (fun li -> MakeTuple li) <*> fold_left_map (foldmap_atom h) li
      | Nth (i, a) -> ret (fun a -> Nth(i, a)) <*> foldmap_atom h a
      | MakeRecord li -> ret (fun li -> MakeRecord li) <*> fold_left_map_snd (foldmap_atom h) li
      | Proj (i, a) -> ret (fun a -> Proj(i, a)) <*> foldmap_atom h a
      | CallDirect (funid, li) -> ret (fun funid li -> CallDirect (funid, li)) <*> g funid <*> fold_left_map (foldmap_atom h) li
      | CallClosure (a, li) -> ret (fun a li -> CallClosure (a, li)) <*> foldmap_atom h a <*> fold_left_map (foldmap_atom h) li
      | MakeClosure (funid, i, li) -> ret (fun funid li -> MakeClosure (funid, i, li)) <*> g funid <*> fold_left_map (foldmap_atom h) li

    let foldmap f g h = function
      | Let (v, rhs, e) -> ret (fun v rhs e -> Let (v, rhs, e)) <*> h v <*> foldmap_rhs foldmap_atom g h rhs <*> f e
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
      natives : string B.Map.t;
      fresh : BT.t;
      globalMap : BT.t B.Map.t;
      localMap : BT.t B.Map.t;
      arity :int  BT.Map.t
  }
  let empty = {natives = B.Map.empty; fresh = BT.init; globalMap = B.Map.empty; localMap = B.Map.empty; arity = BT.Map.empty }

  let b_to_atom b env =
    match B.Map.find_opt b env.localMap with
    | Some s -> Fixable.Var s
    | None ->
       match B.Map.find_opt b env.natives with
       | Some s -> Fixable.Native s
       | None ->
          match B.Map.find_opt b env.globalMap with
          | Some s -> Fixable.GlobalVar s
          | None -> Lib.Pp.failwith "binding failed : %a" B.pp b

  let rec unlambda acc expr =
    match Ast.Expr.Fixed.unfix expr with
    | Ast.Expr.Fixable.Lambda (v, l) ->
       unlambda (v::acc) l
    | _ -> acc, expr

  let unlambda expr =
    let args, body = unlambda [] expr in
    List.rev args, body

  let bind_global env binding arity =
    let binding' = env.fresh in
    let env = { env with fresh = BT.next env.fresh;
                         globalMap = B.Map.add binding binding' env.globalMap;
                         arity = BT.Map.add binding' arity env.arity
              }
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
    | RedLambda of BT.t list * (BT.t list -> BT.t option -> reduce_result)

  let res a env = match a env with
    | RedRes (env, funs, make, atom) -> env, funs, make, atom
    | RedLambda (bindings, fun_) -> fun_ bindings None

  let print_mat f mat =
    Lib.Pp.list (Lib.Pp.sep "@\n") (fun f (li, _addon) ->
        Lib.Pp.list (Lib.Pp.sep " ") (fun f x -> match x with
                                                 | None -> Format.fprintf f " _ "
                                                 | Some Ast.Expr.Pattern.Tuple _li -> Format.fprintf f "(...) "
                                                 | Some Ast.Expr.Pattern.Var binding -> Format.fprintf f "v%a" B.pp binding
                                                 | Some Ast.Expr.Pattern.Construct (a, _) -> Format.fprintf f "C%d(...)" a
      ) f li ) f mat

  let group_patterns env v_in li =
    let fakevar = Fixable.Unit in (* variable jamais utilisée *)
    let find_tuple mat =
      List.find_map (fun (cols, _) ->
          List.fold_left (fun (col, opt) pat ->
              match opt, pat with
              | Some _, _ -> col, opt
              | None, Some Ast.Expr.Pattern.Tuple li -> col + 1, Some (col, List.length li)
              | None, _ -> col + 1, None
            ) (0, None) cols |> snd
          ) mat in
    let unpatvar env vin bind binding =
      let env, binding' = bind_local env binding in (* ça génère un letin inutile qu'on devra corriger à l'inline *)
      let bind' in' = bind (Fixed.fix (Fixable.Let (binding', Fixable.Atom vin, in')) ()) in
      env, bind'
    in
    let pat_to_len (bind, e) env vin pat len = match pat with
      | None -> env, (bind, e), List.map (fun _ -> None) (0 -- len)
      | Some Ast.Expr.Pattern.Tuple li -> env, (bind, e), List.map (fun i -> Some (Ast.Expr.Fixed.unfix_u i)) li
      | Some Ast.Expr.Pattern.Var binding ->
         let env, bind = unpatvar env vin bind binding in
         env, (bind, e), List.map (fun _ -> None) (0 -- len)
      | Some Ast.Expr.Pattern.Construct (_, _) -> assert false
    in
    let unvar_line vins env (line, (bind, e)) =
      let (env, bind), line = List.fold_left_map (fun (env, bind) (pat, vin) ->
                                  match pat with
                                  | Some Ast.Expr.Pattern.Var binding ->
                                     let env, bind = unpatvar env vin bind binding in
                                     (env, bind), None
                                  | x -> (env, bind), x
                                ) (env, bind) (List.combine line vins) in
      env, (line, (bind, e))
    in
    let rec untuple env decl vins mat =
      (* Format.printf "untuple : @[%a@]@\n" print_mat mat; *)
      match find_tuple mat with
      | None ->
         let env, mat = List.fold_left_map (unvar_line vins) env mat in
         env, decl, vins, mat
      | Some (col, len) ->
         let vin = List.nth vins col in (* pour les patVar *)
         let (env, decl, _), vins =
           List.fold_left_map (fun (env, decl, i) vin ->
               if i = col then
                 let env, decl, vins' =
                   List.fold_left (fun (env, decl, vins') i ->
                       let env, binding = fresh env in
                       let decl in' = Fixed.fix (Fixable.Let (binding, Fixable.Nth (i, vin), decl in')) () in
                       (env, decl, (Fixable.Var binding)::vins')) (env, decl, []) (0 -- len)
                 in (env, decl, i+1), List.rev vins'
               else (env, decl, i+1), [vin]
             ) (env, decl, 0) vins
         in
         let vins = List.flatten vins in
         let env, mat = List.fold_left_map (fun env (line, aux) ->
                       let (_, aux, env), line = List.fold_left_map (fun (i, aux, env) pat ->
                                                if i = col then
                                                  let env, aux, pat = pat_to_len aux env vin pat len in
                                                  (i+1, aux, env), pat
                                                else (i+1, aux, env), [pat]) (0, aux, env) line
                       in env, (line, aux)) env mat
         in let mat = List.map (fun (line, aux) -> List.flatten line, aux) mat
         in untuple env decl vins mat in
    let pat_tag = function
      | Ast.Expr.Pattern.Construct (c, _) -> c
      | _ -> assert false in
    let discr_col mat =
      let mat = List.map fst mat in
      match mat with
      | [] -> None
      | hd::_tl -> let cols = List.mapi (fun i _ -> i, List.map (fun l -> List.nth l i) mat) hd in
                   let cols = List.map (fun (nth, col) -> (* calcul d'un score par colone *)
                                  let col = List.filter_map Lib.id col in
                                  let col = List.map pat_tag col in (* que les tags *)
                                  let col = Lib.group_count (Int.compare) col in
                                  let counts = List.map snd col in
                                  let tags = List.map fst col in (* les tags de patterns *)
                                  let score = List.fold_left (fun acc col -> acc * (col + 1)) 1 counts in
                                  nth, score, tags) cols in
                   let cols = List.sort (fun (_, a, _) (_, b, _) -> b - a) cols in
                   match cols with
                   | (_, 1, _)::_ -> None (* Que des None, aucun pattern *)
                   | x::_ -> Some x
                   | [] -> None
    in
    let selected_lines env mat vins col field = (* on spécialise la colone `col` pour le champ `field`, il faut déclarer l'accès au sous pattern et virer les lignes ou le constructeur n'est pas le bon.*)
      let env, binding = fresh env in
      let vin = List.nth vins col in
      let make in' = Fixed.fix (Fixable.Let (binding, (Fixable.Destruct (field, vin)), in')) () in
      let vins = List.mapi (fun i v -> if i = col then Fixable.Var binding else v) vins in
      let vin' = List.nth vins col in (* le binding de la nouvelle colone après accès au contenu du constructeur *)
      let env, mat = List.fold_left_map (fun env (line, (bind, e)) ->
                    match List.nth line col with
                    | Some (Ast.Expr.Pattern.Var _)
                    | Some (Ast.Expr.Pattern.Tuple _) -> assert false (* sont gérés par untuple *)
                    | Some (Ast.Expr.Pattern.Construct (field', p)) ->
                            if field = field' then
                              match Ast.Expr.Fixed.unfix_u p with
                              | Ast.Expr.Pattern.Var binding ->
                                 let env, bind = unpatvar env vin' bind binding in
                                 env, Some (List.mapi (fun i p' -> if i = col then None else p') line, (bind, e))
                              | pat -> env, Some (List.mapi (fun i p' -> if i = col then Some pat else p') line, (bind, e))
                            else env, None
                    | None -> env, Some (line, (bind, e))
                  ) env mat in
      let mat = List.filter_map Fun.id mat in
      env, make, vins, mat in
    let else_lines mat col = (* même fonction, mais pour les cas ou aucune ligne n'a matché les champs *)
      List.filter_map (fun (line, e) ->
          match List.nth line col with
          | Some (Ast.Expr.Pattern.Var _)
            | Some (Ast.Expr.Pattern.Tuple _) -> assert false (* sont gérés par untuple *)
          | Some (Ast.Expr.Pattern.Construct _) -> None
          | None -> Some (line, e)
        ) mat in
    let pat_failed = Fixed.fix Fixable.AssertFalse () in
    let ret binding_out =  Fixed.fix (Fixable.Return binding_out) () in
    let rec compile env vins mat =
      let env, decl, vins, mat = untuple env Lib.id vins mat in
      match discr_col mat with
      | None -> begin match mat with (* TODO ajouter déclarations *)
                  | [] -> env, [], fakevar, (fun _in' -> pat_failed)
                  | [(_hd, (bind, e))] ->
                     let env, functions, make, e = res e env in
                     env, functions, e, decl << bind << make
                  | (_hd, (bind, e))::_tl ->
                     let env, functions, make, e = res e env in
                     env, functions, e, decl << bind << make (* TODO warn *)
                end
      | Some (col, _, fields) ->
         let vin: BT.t Fixable.atom = List.nth vins col in
         let (env, functions), tests = List.fold_left_map (fun (env, functions') field ->
                         let env, binding_out = fresh env in
                         let env, decl_ok, vins, mat = selected_lines env mat vins col field in
                         let env, functions, binding_out_ok, make = compile env vins mat in
                         let functions = List.rev_append functions functions' in
                         let ok = decl_ok (make (ret binding_out_ok)) in
                         (env, functions), ((Fixable.Var binding_out), fun in' nok -> Fixed.fix (Fixable.LetIfTag(binding_out, vin, field, ok, nok, in')) ())) (env, []) fields
         in match tests with
            | (binding0, make0)::tl ->
               let mat_else = else_lines mat col in
               let env, functions', binding_else, make_else = compile env vins mat_else in
               let nok = List.fold_left (fun nok (binding_out, make) ->
                             make (ret binding_out) nok) (make_else (ret binding_else)) tl in
               env, List.rev_append functions' functions, binding0, fun in' -> decl (make0 in' nok)
            | _ -> assert false
    in
    let vins, mat = [v_in], List.map (fun (p, e) -> [Some (Ast.Expr.Fixed.unfix_u p)], (Fun.id, e)) li in
    compile env vins mat

  let reduce_expr used_vars expr env =
    match expr with
    | Ast.Expr.Fixable.Integer i -> RedRes(env, [], id, Fixable.Int i)
    | Ast.Expr.Fixable.Bool i -> RedRes(env, [], id, Fixable.Bool i)
    | Ast.Expr.Fixable.Char c -> RedRes(env, [], id, Fixable.Char c)
    | Ast.Expr.Fixable.Var v -> (* si la variable est une fonction globale toplvl, on est obligé d'en créer une cloture *)
       begin match B.Map.find_opt v env.globalMap with
       | None -> RedRes(env, [], id, b_to_atom v env)
       | Some s -> match BT.Map.find_opt s env.arity with
          | None -> Lib.Pp.failwith "(* bug *) arity not found %a (from : %a)" BT.pp s B.pp v
          | Some 0 -> RedRes(env, [], id, b_to_atom v env)
          | Some arity ->
             let env, c = fresh env in
             RedRes(env, [], (fun in' -> Fixable.Let (c, Fixable.MakeClosure(s, arity, []), in') |> fix), Fixable.Var c)
      end
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

    | Ast.Expr.Fixable.LetRecIn (binding, a, b) ->
       let env, bindingrec = bind_local env binding in
       begin match a env with
         | RedLambda (bindings, fun_) ->
            let env, functionsa, makea, a' = fun_ bindings (Some bindingrec) in
            let env, binding' = bind_local env binding in
            let env, functionsb, makeb, b' = res b env in
            RedRes(env, List.flatten [functionsa; functionsb], makea << (fun in_ ->
                     Fixable.Let (binding', Atom a', makeb in_) |> fix), b')
         | RedRes (_env, _functionsa, _makea, _a' ) -> assert false
       end
    | Ast.Expr.Fixable.LetIn (binding, a, b) ->
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
       RedLambda ([binding'], (fun bindings rec_option -> (* TODO gérer la récursion sur cette variable *)
                    let env, funid = fresh env in
                    let clot_vars = List.filter_map (fun binding ->
                                        Option.bind
                                          (B.Map.find_opt binding env.localMap)
                                          (fun b -> if List.mem b bindings then None else Some b)
                                      )
                                      (B.Set.to_list used_vars) in
                    let clot_vars = List.filter (fun v -> Some v <> rec_option) clot_vars in
                    let clot_vars_atoms = List.map (fun v -> Fixable.Var v) clot_vars in
                    let env, v = fresh env in
                    let args = List.append clot_vars bindings in
                    let arity = List.length args in
                    let makea = match rec_option with
                      | None -> makea
                      | Some binding ->
                         fun in_ -> Fixable.Let (binding, Fixable.MakeClosure (funid, arity, clot_vars_atoms), makea in_) |> fix
                    in
                    let funs = {id=funid;
                                args=args;
                                body=makereturn makea a'} in
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
       let env, functions', binding, make' = group_patterns env e li in
       let functions = List.rev_append functions functions' in
       let make in' = make (make' in') in
       RedRes(env, functions, make, binding)


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
    | Ast.Expr.Fixable.LetRecIn (v, e1, e2)
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
    let env, functions, makee, atom = res (Ast.Expr.Fixed.Deep.reduce_de reduce_expr expr) env in
     env, functions, makereturn makee atom

  let process env toplvl =
    match toplvl with
    | Ast.TopLvl.External (binding, _ty, s) -> {env with natives = B.Map.add binding s env.natives}, {functions=[]; entry=[]}
    | Ast.TopLvl.TypeDecl (_tylist, _binding, _ty) -> env, {functions=[]; entry=[]}
    | Ast.TopLvl.Let (binding, expr)
    | Ast.TopLvl.LetRec (binding, expr) ->
       let args, body = unlambda expr in
       let env, args = List.fold_left_map (fun env binding -> bind_local env binding) env args in
       let env, functions, body = process env body in
       let env, binding' = bind_global env binding (List.length args) in
       match args with
       | [] -> env, {functions=functions; entry=[binding', body]}
       | li ->
          env, {functions=List.append functions [{
                                           id = binding';
                                           args = li;
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
