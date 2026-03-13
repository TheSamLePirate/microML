open Ast

let process expr = Expr.Fixed.Deep.map
                     (fun e -> match e with
                               | Expr.Fixable.BinOp (a, Expr.And, b) -> Expr.Fixable.IfThenElse (a, b, Expr.Fixed.fix (Expr.Fixable.Bool false) (Expr.Fixed.decoration b) )
                               | Expr.Fixable.BinOp (a, Expr.Or, b) -> Expr.Fixable.IfThenElse (a, Expr.Fixed.fix (Expr.Fixable.Bool true) (Expr.Fixed.decoration b), b)
                               | x -> x
                     ) expr

let process () toplvl =
  let open TopLvl in
  match toplvl with
  | External _ | TypeDecl _ -> (), toplvl
  | Let (binding, expr) -> let expr = process expr in (), Let(binding, expr)
  | LetRec( binding, expr) -> let expr = process expr in (), LetRec(binding, expr)

  let passe = {
      Ast.init = ();
      Ast.process = process;
  }
  let debug f_pp b_pp tyv_pp tyb_pp f e = Ast.TopLvl.pp f_pp b_pp tyv_pp tyb_pp f e
