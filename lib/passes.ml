open Lib

module StringBindings : Ast.Bindings with type t = string = struct
  include String
  let pp = Pp.string
end
module IntBindings : Ast.FreshableBindings with type t = int = struct
  include Int
  let pp = Pp.int
  let init = 0
  let next n = n + 1
end
module StringFields : Ast.Fields with type t = string = struct
  include String
  let pp = Pp.string
end
module IntFields : Ast.Fields with type t = int = struct
  include Int
  let pp = Pp.int
end

module Bindings = Bindings.Make(StringBindings)(IntBindings)
module Ty = Typer.Make(StringBindings)(IntBindings)(StringFields)
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

