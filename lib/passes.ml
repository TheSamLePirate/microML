open Lib
open Ast

let bindings e =
  let stringmap_add (stringmap, indice) binding = indice, (StringMap.add binding indice stringmap, indice + 1) in
  let open Expr.Fixed in
  let open Expr.Fixable in
  let _, e = Traverse.foldmap (fun tra acc e ->
     match e with
     | Lambda (binding, in_) ->
        let ibinding, acc' = stringmap_add acc binding in
        let _, in_ = wrapAcc tra in_ acc' in
        acc, Lambda (ibinding, in_)
     | LetIn (binding, def, in_) ->
        let _, def = wrapAcc tra def acc in
        let ibinding, acc' = stringmap_add acc binding in
        let _, in_ = wrapAcc tra in_ acc' in
        acc, LetIn (ibinding, def, in_)
     | LetRecIn (binding, def, in_) ->
        let _, def = wrapAcc tra def acc in
        let ibinding, acc' = stringmap_add acc binding in
        let _, in_ = wrapAcc tra in_ acc' in
        acc, LetRecIn (ibinding, def, in_)
     | x -> tra x acc
               )
               (fun tra acc p -> tra p acc)
               (fun x acc -> acc, x)
               (fun x ((map, _) as acc) -> acc, StringMap.find x map)
               e
               (StringMap.empty, 0)
  in e
