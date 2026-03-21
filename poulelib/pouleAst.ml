module Fix3 = LibCore.Lib.Fix3
module Fix4 = LibCore.Lib.Fix4
module Fix5 = LibCore.Lib.Fix5
module Ident = LibCore.Ident

module Term = struct
  module Fixed = Fix4(Fixable.Term)
  let fix loc node = Fixed.fix node loc
end

module Ty = struct
  module Fixed = Fix3(Fixable.Ty)
  let fix loc node = Fixed.fix node loc
end

module Formula = struct
  module Fixed = Fix5(Fixable.Formula)
  let fix loc node = Fixed.fix node loc
end
module ProofTerm = struct
  module Fixed = Fix5(Fixable.ProofTerm)
  let fix loc node = Fixed.fix node loc
end

module TopLevel = struct
  type ('ty, 'term, 'formula, 'proof, 'name) t =
    | TypeDecl of {
        name : 'name;
        params : 'name list;
        constructors : ('name * 'ty list) list;
      }
    | FunDecl of {
        name : 'name;
        args : 'ty list;
        ret : 'ty;
      }
    | PredDecl of {
        name : 'name;
        args : 'ty list;
      }
    | Axiom of {
        name : 'name;
        formula : 'formula;
      }
    | Theorem of {
        name : 'name;
        formula : 'formula;
        proof : 'proof;
      }
    | Check of {
        proof : 'proof;
        formula : 'formula;
      }
end

module Prog
         (V : Ident.Bindings)
         (C : Ident.Fields)
         (F : Ident.Bindings)
         (A : sig type deco type pred end)
  = struct
  module Var = V
  module Constr = C
  module Funsym = F
  type deco = A.deco
  type pred = A.pred
  type ty = (deco, Funsym.t, Var.t) Ty.Fixed.t
  type term = (deco, Var.t, Constr.t, Funsym.t) Term.Fixed.t
  type formula = (deco, ty, Var.t, term, pred) Formula.Fixed.t
  type proof = (deco, formula, ty, term, Var.t) ProofTerm.Fixed.t
end
