
open Miniml

let () =
  let filename = Sys.argv.(1) in
  let ast = Passes.parse_file filename in
  let ast = Passes.Ty.process ast in
  let ty = Ast.Expr.Fixed.decoration ast in
  (*
  Format.printf "ast : %a@\n" (Ast.Expr.Fixed.pp
                                 Passes.StringFields.pp
                                 Passes.StringFields.pp
    ) ast;
  *)
  let ast_e = Passes.Bindings.process ast in
  let value = Passes.EvalI.compile ast_e Passes.EvalI.empty_env in
  (*   Format.printf "%a@\n" Passes.EvalI.pp value *)
  Format.printf "%a : %a@\n" Passes.EvalI.pp value
    (Ast.Ty.Fixed.pp
       Lib.Pp.string
       Ast.Ty.pp_tyvar
       Lib.Pp.string
    ) ty
