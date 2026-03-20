module ParsedProg : PouleAst.Prog = struct

  module Var = Miniml.Ident.StringBindings
  module Constr = Miniml.Ident.StringBindings
  module Funsym = Miniml.Ident.StringBindings

  type deco = Miniml.Lib.Loc.t
  type ty = (deco, Funsym.t, Var.t) PouleAst.Ty.Fixed.t
  type term = (deco, Var.t, Constr.t, Funsym.t) PouleAst.Term.Fixed.t
  type pred = Var.t
  type formula = (deco, ty, Var.t, term, pred) PouleAst.Formula.Fixed.t
  type proof = (deco, formula, ty, term, Var.t) PouleAst.ProofTerm.Fixed.t
  type program = (ty, term, formula, proof, string) PouleAst.TopLevel.t list

end

