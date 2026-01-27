open Ast
open Lib

(* module de conversion de fields à partir du type B.t vers BTarget.t *)

module Make(B:Fields)(BTarget:Ast.FreshableFields) = struct

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
       let acc, ty = Ty.Fixed.Deep.foldmapfgh process idacc idacc ty acc in
       acc, TypeDecl (tylist, binding_type, ty)
    | Let (binding, expr) ->
       let acc, expr = Expr.Fixed.Deep.foldmapfg process idacc expr acc in
       acc, Let(binding, expr)
    | LetRec( binding, expr) ->
       let acc, expr = Expr.Fixed.Deep.foldmapfg process idacc expr acc in
       acc, LetRec(binding, expr)

  let passe = {
      Ast.init = B.Map.empty, BTarget.init;
      Ast.process = process;
  }
  let debug f_pp tyv_pp tyb_pp f e = Ast.TopLvl.pp f_pp BTarget.pp tyv_pp tyb_pp f e

end
