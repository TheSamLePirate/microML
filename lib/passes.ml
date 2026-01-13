open Lib
open Ast

(* bindings de string vers int *)
let bindings e =
  let stringmap_add (stringmap, indice) binding = indice, (StringMap.add binding indice stringmap, indice + 1) in
  let open Expr.Fixed in
  let open Expr.Fixable in
  let _, e = Traverse.foldmap (fun fin tra acc e ->
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
        let _, def = fin def acc in
        let _, in_ = fin in_ acc' in
        acc, LetRecIn (ibinding, def, in_)
     | x ->
        tra x acc
               )
               (fun _fin tra acc p -> tra p acc) (* TODO *)
               (fun x acc -> acc, x)
               (fun x ((map, _) as acc) ->
                 acc, StringMap.find x map)
               e
               (StringMap.empty, 0)
  in e


module StringBindings : Ast.Bindings with type t = string = struct
  include String
  let pp = Pp.string
end
module IntBindings : Ast.Bindings with type t = int = struct
  include Int
  let pp = Pp.int
end
module StringFields : Ast.Fields with type t = string = struct
  include String
  let pp = Pp.string
end
module IntFields : Ast.Fields with type t = int = struct
  include Int
  let pp = Pp.int
end

module Ty = Typer.Make(StringBindings)(StringFields)
module EvalI = Eval.Make(IntBindings)(StringFields)

let parse_file filename =
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = filename };
  let ast =
    try Parser.main Lexer.token lb
    with
    | Parser.Error ->
        let p = lb.lex_curr_p in
        failwith
          (Printf.sprintf "parse error at %s:%d:%d"
             p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol))
  in
  close_in ic;
  ast

