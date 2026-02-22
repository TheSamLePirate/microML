open Ast

module Make(B:Bindings)(BTarget:Ast.FreshableBindings) :
sig
  type acc
  val passe : (acc,
               ('a, 'b, 'c, 'd, 'e, B.t) Ast.TopLvl.decl,
               ('a, 'b, 'c, 'd, 'e, BTarget.t) Ast.TopLvl.decl) passe
  val debug : (Format.formatter -> 'field -> unit) ->
              (Format.formatter -> 'tyvar -> unit) ->
              (Format.formatter -> 'tybinding -> unit) ->
              Format.formatter ->
              ('a, 'b, 'tyvar, 'tybinding, 'field, BTarget.t) Ast.TopLvl.decl -> unit
end = struct

  type acc = BTarget.t B.Map.t * BTarget.t

  let stringmap_add (stringmap, indice) binding =
    indice, (B.Map.add binding indice stringmap, BTarget.next indice)

  let justfresh (_, fresh) (map, _) = map, fresh
  let jf acc (acc', a) = justfresh acc' acc, a

  (* bindings de B.t vers Btarget.t *)
  let process e acc =
  let open Expr.Fixed in
  let open Expr.Fixable in
  let acc, e = Traverse.foldmap (fun fin gin tra acc e ->
     match e with
     | Lambda (binding, in_) ->
        let ibinding, acc' = stringmap_add acc binding in
        let acc, in_ = fin in_ acc' |> jf acc in
        acc, Lambda (ibinding, in_)
     | LetIn (binding, def, in_) ->
        let acc, def = fin def acc |> jf acc in
        let ibinding, acc' = stringmap_add acc binding in
        let acc, in_ = fin in_ acc' |> jf acc in
        acc, LetIn (ibinding, def, in_)
     | LetRecIn (binding, def, in_) ->
        let ibinding, acc' = stringmap_add acc binding in
        let acc, def = fin def acc' |> jf acc in
        let acc, in_ = fin in_ acc' |> jf acc in
        acc, LetRecIn (ibinding, def, in_)
     | PatternMatch (a, li) ->
        let acc, a = fin a acc |> jf acc in
        let acc, li = List.fold_left_map (fun acc (pattern, expr) ->
                        let acc', pattern = gin pattern acc in
                        let acc, expr = fin expr acc' |> jf acc in
                        acc, (pattern, expr)
                      ) acc li in
        acc, PatternMatch (a, li)
     | x ->
        tra x acc
               )
               (fun _fin tra acc p ->
                 match p with
                 | Expr.Pattern.Var binding ->
                    let ibinding, acc' = stringmap_add acc binding in
                    acc', Expr.Pattern.Var ibinding
                 | _ ->  tra p acc)
               (fun x acc -> acc, x)
               (fun binding ((map, _) as acc) ->
(*                 Format.printf "search %a in %a@\n" B.pp binding (Lib.Pp.assoc B.pp BTarget.pp) (BMap.to_list map); *)
                 (* TODO gérer l'erreur *)
                 acc, B.Map.find binding map)
               e
             acc
  in acc, e

  let process acc toplvl =
    let open Ast.TopLvl in
    match toplvl with
    | TypeDecl (tylist, binding_type, ty) ->
       acc, TypeDecl (tylist, binding_type, ty)
    | Let (binding, expr) ->
       let acc, expr = process expr acc in
       let ibinding, acc' = stringmap_add acc binding in
       acc', Let(ibinding, expr)
    | LetRec( binding, expr) ->
       let ibinding, acc' = stringmap_add acc binding in
       let acc', expr = process expr acc' in
       acc', LetRec(ibinding, expr)
    | External (binding, ty, str) ->
       let binding', acc = stringmap_add acc binding in
       acc, External (binding', ty, str)

  let passe = {
      Ast.init = B.Map.empty, BTarget.init;
      Ast.process = process;
  }
  let debug f_pp tyv_pp tyb_pp f e = Ast.TopLvl.pp f_pp BTarget.pp tyv_pp tyb_pp f e
end
