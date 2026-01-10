open Ast
open Lib

let () =
  let open Expr in
  let fix a = Fixed.fix a () in
  let test_e = fix (LetIn ("x", fix (Integer 1), fix (BinOp ( fix (Var "x"), Add, fix (Integer 1 )))))
  in
  let test_e_int = Passes.bindings test_e in
  let () = Format.printf "test = %a@\n" (Expr.Fixed.pp Pp.string Pp.string) test_e in
  let () = Format.printf "test = %a@\n" (Expr.Fixed.pp Pp.string (Pp.prefix "v" Pp.int)) test_e_int
  in ()

