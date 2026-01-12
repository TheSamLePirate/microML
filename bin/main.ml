open Ast
open Lib

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

let () =
  let open Expr in
  let fix a = Fixed.fix a () in
  let test_e = fix (LetIn ("x", fix (Integer 1), fix (BinOp ( fix (Var "x"), Add, fix (Integer 1 )))))
  in
  let test_e_int = Passes.bindings test_e in
  let () = Format.printf "test = %a@\n" (Expr.Fixed.pp Pp.string Pp.string) test_e in
  let () = Format.printf "test = %a@\n" (Expr.Fixed.pp Pp.string (Pp.prefix "v" Pp.int)) test_e_int in
  let value = EvalI.compile test_e_int EvalI.empty_env in
  let () = Format.printf "value = %a@\n" EvalI.pp value in
  ()

