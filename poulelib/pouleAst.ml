open LibCore

module Term = struct
  module Fixed = Fix4.Fix(Fixable.Term)
  let fix loc node = Fixed.fix node loc
end

module Ty = struct
  module Fixed = Fix3.Fix(Fixable.Ty)
  let fix loc node = Fixed.fix node loc
end

module Formula = struct
  module Fixed = Fix5.Fix(Fixable.Formula)
  let fix loc node = Fixed.fix node loc
end
module ProofTerm = struct
  module Fixed = Fix5.Fix(Fixable.ProofTerm)
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
        name : 'name option;
        formula : 'formula;
        proof : 'proof;
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
    | Theorem {name=Some name; formula; proof} -> Format.fprintf f "@[<v 2>theorem %a :@\n%a@][<v 2>=@\n%a]"
                                      pp_name name pp_formula formula pp_proof proof
    | Theorem {name = None; proof; formula} -> Format.fprintf f "check %a@\n[<v 2>:@\n%a@]" pp_proof proof pp_formula formula
end
