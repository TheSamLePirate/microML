open Ast

(* module de conversion de fields à partir du type B.t vers BTarget.t *)

module Make(B:LibCore.Ident.Fields)(BTarget:LibCore.Ident.FreshableFields) = struct

  type acc = BTarget.t B.Map.t * BTarget.t

  let process field (map, next) =
    match B.Map.find_opt field map with
    | None ->
       let next' = BTarget.next next in
       let map = B.Map.add field next map in
       (map, next'), next
    | Some s ->
       (map, next), s

  let process acc toplvl =
    let open Ast.TopLvl in
    match toplvl with
    | TypeDecl (tylist, binding_type, ty) ->
       let acc, ty = Ty.Fixed.Deep.foldmapfgh process LibCore.Aux.idacc LibCore.Aux.idacc ty acc in
       acc, TypeDecl (tylist, binding_type, ty)
    | Let (binding, expr) ->
       let acc, expr = Expr.Fixed.Deep.foldmapfg process LibCore.Aux.idacc expr acc in
       acc, Let(binding, expr)
    | LetRec( binding, expr) ->
       let acc, expr = Expr.Fixed.Deep.foldmapfg process LibCore.Aux.idacc expr acc in
       acc, LetRec(binding, expr)
    | External (binding, ty, s) ->
       let acc, ty = Ty.Fixed.Deep.foldmapfgh process LibCore.Aux.idacc LibCore.Aux.idacc ty acc in
       acc, External (binding, ty, s)

  let passe = { LibCore.Passes.init = B.Map.empty, BTarget.init; process = process; }
  let debug b_pp tyv_pp tyb_pp f e = Ast.TopLvl.pp BTarget.pp b_pp tyv_pp tyb_pp f e

end
