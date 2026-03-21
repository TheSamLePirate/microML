module Fix3 = LibCore.Lib.Fix3
module Fix4 = LibCore.Lib.Fix4
module Fix5 = LibCore.Lib.Fix5
module Pp = LibCore.Pp
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
  let pp pp_ty pp_formula pp_proof pp_name f = function
    | TypeDecl {name; params; constructors} -> Format.fprintf f "type %a(%a) = %a"
                                                 pp_name name
                                                 (Pp.list (Pp.sep ",@ ") pp_name) params
                                                 (Pp.list (Pp.sep "|@\n") (fun f (name, tylist) -> Format.fprintf f "%a(%a)" pp_name name (Pp.list (Pp.sep ",@ ") pp_ty) tylist )) constructors
    | FunDecl {name; args; ret} -> Format.fprintf f "func %a : (%a) => %a" pp_name name
                                 (Pp.list (Pp.sep ",@ ") pp_ty) args
                                 pp_ty ret
    | PredDecl {name; args} -> Format.fprintf f "pred %a : (%a)" pp_name name
                             (Pp.list (Pp.sep ",@ ") pp_ty) args
    | Axiom {name; formula} -> Format.fprintf f "axiom %a : %a" pp_name name pp_formula formula
    | Theorem {name; formula; proof} -> Format.fprintf f "@[<v 2>theorem %a :@\n%a@][<v 2>=@\n%a]"
                                      pp_name name pp_formula formula pp_proof proof
    | Check {proof; formula} -> Format.fprintf f "check %a@\n[<v 2>:@\n%a@]" pp_proof proof pp_formula formula
end

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
  type ty = (deco, Funsym.t, Var.t) Ty.Fixed.t
  type term = (deco, Var.t, Constr.t, Funsym.t) Term.Fixed.t
  type formula = (deco, ty, Var.t, term, pred) Formula.Fixed.t
  type proof = (deco, formula, ty, term, Var.t) ProofTerm.Fixed.t

  let pp_ty f x = Ty.Fixed.pp Funsym.pp Var.pp f x
  let pp_term f x = Term.Fixed.pp Var.pp Constr.pp Funsym.pp f x
  let pp_formula f x = Formula.Fixed.pp pp_ty Var.pp pp_term A.pp_pred f x
  let pp_proof f x = ProofTerm.Fixed.pp pp_formula pp_ty pp_term Var.pp f x
  let pp f x = TopLevel.pp pp_ty pp_formula pp_proof F.pp f x
end
