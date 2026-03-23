module Loc = LibCore.Loc
module Ident = LibCore.Ident
module Pp = LibCore.Pp

module Prog
         (V : Ident.Bindings)
         (C : Ident.Fields)
         (F : Ident.Bindings)
         (A : sig
            type deco
            type pred
            val pp_pred : pred Pp.printer
          end)
  = struct
  module Var = V
  module Constr = C
  module Funsym = F
  type deco = A.deco
  type pred = A.pred
  type ty = (deco, Funsym.t, Var.t) PouleAst.Ty.Fixed.t
  type term = (deco, Var.t, Constr.t, Funsym.t) PouleAst.Term.Fixed.t
  type formula = (deco, ty, Var.t, term, pred) PouleAst.Formula.Fixed.t
  type proof = (deco, formula, ty, term, Var.t) PouleAst.ProofTerm.Fixed.t

  let pp_ty f x = PouleAst.Ty.Fixed.pp Funsym.pp Var.pp f x
  let pp_term f x = PouleAst.Term.Fixed.pp Var.pp Constr.pp Funsym.pp f x
  let pp_formula f x = PouleAst.Formula.Fixed.pp pp_ty Var.pp pp_term A.pp_pred f x
  let pp_proof f x = PouleAst.ProofTerm.Fixed.pp pp_formula pp_ty pp_term Var.pp f x
  let pp f x = PouleAst.TopLevel.pp pp_ty pp_formula pp_proof F.pp f x
end


module ParsedProg = Prog (Ident.StringBindings) (Ident.StringBindings) (Ident.StringBindings)
                      (struct
                        type deco = Loc.t
                        type pred = string
                        let pp_pred = LibCore.Pp.string
                      end)
