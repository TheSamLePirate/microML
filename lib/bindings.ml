open Ast

module Make(B:Bindings)(BTarget:Ast.FreshableBindings) = struct
  module BMap = Map.Make(B)

(* bindings de string vers int *)
  let process e =
  let stringmap_add (stringmap, indice) binding =
    indice, (BMap.add binding indice stringmap, BTarget.next indice) in
  let open Expr.Fixed in
  let open Expr.Fixable in
  let _, e = Traverse.foldmap (fun fin gin tra acc e ->
     match e with
     | Lambda (binding, in_) ->
        let ibinding, acc' = stringmap_add acc binding in
        let _, in_ = fin in_ acc' in
        acc, Lambda (ibinding, in_)
     | LetIn (binding, def, in_) ->
        let _, def = fin def acc in
        let ibinding, acc' = stringmap_add acc binding in
        let _, in_ = fin in_ acc' in
        acc, LetIn (ibinding, def, in_)
     | LetRecIn (binding, def, in_) ->
        let ibinding, acc' = stringmap_add acc binding in
        let _, def = fin def acc' in
        let _, in_ = fin in_ acc' in
        acc, LetRecIn (ibinding, def, in_)
     | PatternMatch (a, li) ->
        let _, a = fin a acc in
        let li = List.map (fun (pattern, expr) ->
                        let acc, pattern = gin pattern acc in
                        let _, expr = fin expr acc in
                        pattern, expr
                      ) li in
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
                 acc, BMap.find binding map)
               e
               (BMap.empty, BTarget.init)
  in e

end
