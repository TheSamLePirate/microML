open Ast

module Make(B:Ident.Bindings)(BTarget:Ident.FreshableBindings) :
sig
  type acc
  val passe : (acc,
               (Lib.Loc.t, 'b, 'c, 'd, 'e, B.t) Ast.TopLvl.decl,
               (Lib.Loc.t, 'b, 'c, 'd, 'e, BTarget.t) Ast.TopLvl.decl) passe
  val debug : (Format.formatter -> 'field -> unit) ->
              (Format.formatter -> 'tyvar -> unit) ->
              (Format.formatter -> 'tybinding -> unit) ->
              Format.formatter ->
              (Lib.Loc.t, 'b, 'tyvar, 'tybinding, 'field, BTarget.t) Ast.TopLvl.decl -> unit
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
  let acc, e = Traverse.decorate_foldmap (fun fin gin tra acc d e ->
     match e with
     | Lambda (binding, in_) ->
        let ibinding, acc' = stringmap_add acc binding in
        let acc, in_ = fin in_ acc' |> jf acc in
        acc, d, Lambda (ibinding, in_)
     | LetIn (binding, def, in_) ->
        let acc, def = fin def acc |> jf acc in
        let ibinding, acc' = stringmap_add acc binding in
        let acc, in_ = fin in_ acc' |> jf acc in
        acc, d, LetIn (ibinding, def, in_)
     | LetRecIn (binding, def, in_) ->
        let ibinding, acc' = stringmap_add acc binding in
        let acc'', def = fin def acc' |> jf acc in
        let acc' = justfresh acc'' acc' in
        let acc, in_ = fin in_ acc' |> jf acc in
        acc, d, LetRecIn (ibinding, def, in_)
     | PatternMatch (a, li) ->
        let acc, a = fin a acc |> jf acc in
        let acc, li = List.fold_left_map (fun acc (pattern, expr) ->
                        let acc', pattern = gin pattern acc in
                        let acc, expr = fin expr acc' |> jf acc in
                        acc, (pattern, expr)
                      ) acc li in
        acc, d, PatternMatch (a, li)
     | x -> let acc, x = tra x acc in acc, d, x)
               (fun _fin tra acc d p ->
                 match p with
                 | Expr.Pattern.Var binding ->
                    let ibinding, acc' = stringmap_add acc binding in
                    acc', d, Expr.Pattern.Var ibinding
                 | _ ->  let acc, x = tra p acc in acc, d, x)
               (fun _d x acc -> acc, x)
               (fun location binding ((map, _) as acc) -> match B.Map.find_opt binding map with
                                                 | None -> Lib.Pp.failwith "unbound value %a at %a" B.pp binding Lib.Loc.pp location
                                                 | Some s -> acc, s)
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
