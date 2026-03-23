
/* parser.mly */

%{

let mk_loc = LibCore.Loc.loc

let fix_ty sp ep node = PouleAst.Ty.fix (mk_loc sp ep) node
let fix_term sp ep node = PouleAst.Term.fix (mk_loc sp ep) node
let fix_formula sp ep node = PouleAst.Formula.fix (mk_loc sp ep) node
let fix_proof sp ep node = PouleAst.ProofTerm.fix (mk_loc sp ep) node
let mk_arrow_ty sp ep args ret = fix_ty sp ep (Fixable.Ty.TyArrow (args, ret))

let mk_fun_decl name args ret = PouleAst.TopLevel.FunDecl { name; args; ret }
let mk_pred_decl name args = PouleAst.TopLevel.PredDecl { name; args }
let mk_axiom name formula = PouleAst.TopLevel.Axiom { name; formula }
let mk_theorem name formula proof = PouleAst.TopLevel.Theorem { name=Some name; formula; proof }
let mk_check proof formula = PouleAst.TopLevel.Theorem { name=None; proof; formula }
let mk_type_decl name params constructors = PouleAst.TopLevel.TypeDecl { name; params; constructors }
%}

%token <string> LIDENT
%token <string> UIDENT
%token <string> TVAR

%token TYPE
%token FUNDECL
%token PREDDECL
%token AXIOM
%token THEOREM
%token CHECK

%token FORALL
%token EXISTS
%token FUN
%token GEN
%token MATCH
%token WITH
%token LEFT
%token RIGHT
%token FST
%token SND
%token TRIV
%token ABORT
%token PACK
%token UNPACK
%token TOP
%token BOTTOM
%token OF
%token DARROW
%token ARROW
%token AND
%token OR

%token COLON
%token DOT
%token COMMA
%token BAR
%token EQ
%token STAR
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK

%token EOF

%right ARROW
%left OR
%left AND
%left EQ
%left APP

%start <(Types.ParsedProg.ty, Types.ParsedProg.term, Types.ParsedProg.formula, Types.ParsedProg.proof, string) PouleAst.TopLevel.t list> main

%%

main:
  | decls EOF
      { $1 }

decls:
  |
      { [] }
  | decl decls
      { $1 :: $2 }

decl:
  | TYPE type_params_opt UIDENT EQ ctor_decls
      { mk_type_decl $3 $2 $5 }

  | FUNDECL LIDENT COLON fun_sig
      { let (args, ret) = $4 in mk_fun_decl $2 args ret }

  | PREDDECL LIDENT COLON pred_sig
      { mk_pred_decl $2 $4 }

  | AXIOM LIDENT COLON formula
      { mk_axiom $2 $4 }

  | THEOREM LIDENT COLON formula EQ proof
      { mk_theorem $2 $4 $6 }

  | CHECK proof COLON formula
      { mk_check $2 $4 }

type_params_opt:
  |
      { [] }
  | type_params
      { $1 }

type_params:
  | TVAR
      { [$1] }
  | TVAR type_params
      { $1 :: $2 }

ctor_decls:
  | ctor_decl
      { [$1] }
  | ctor_decl BAR ctor_decls
      { $1 :: $3 }

ctor_decl:
  | UIDENT
      { ($1, []) }
  | UIDENT OF ctor_arg_list
      { ($1, $3) }

ctor_arg_list:
  | ty
      { [$1] }
  | ty STAR ctor_arg_list
      { $1 :: $3 }

fun_sig:
  | LPAREN ty_list_opt RPAREN ARROW ty
      { ($2, $5) }

pred_sig:
  | LPAREN ty_list_opt RPAREN
      { $2 }

ty_list_opt:
  |
      { [] }
  | ty_list
      { $1 }

ty_list:
  | ty
      { [$1] }
  | ty COMMA ty_list
      { $1 :: $3 }

ty:
  | ty_atom ARROW ty
      { mk_arrow_ty $startpos $endpos [$1] $3 }
  | LPAREN ty_list RPAREN ARROW ty
      { mk_arrow_ty $startpos $endpos $2 $5 }
  | ty_atom
      { $1 }

ty_atom:
  | TVAR
      { fix_ty $startpos $endpos (Fixable.Ty.TyVar $1) }

  | UIDENT
      { fix_ty $startpos $endpos (Fixable.Ty.TyNamed ($1, [])) }

  | UIDENT LPAREN ty_list_opt RPAREN
      { fix_ty $startpos $endpos (Fixable.Ty.TyNamed ($1, $3)) }

  | LIDENT
      { fix_ty $startpos $endpos (Fixable.Ty.TyNamed ($1, [])) }

  | LIDENT LPAREN ty_list_opt RPAREN
      { fix_ty $startpos $endpos (Fixable.Ty.TyNamed ($1, $3)) }

  | LPAREN ty RPAREN
      { $2 }

formula:
  | formula_impl
      { $1 }

formula_impl:
  | formula_or ARROW formula_impl
      { fix_formula $startpos $endpos (Fixable.Formula.Impl ($1, $3)) }
  | formula_or
      { $1 }

formula_or:
  | formula_or OR formula_and
      { fix_formula $startpos $endpos (Fixable.Formula.Or ($1, $3)) }
  | formula_and
      { $1 }

formula_and:
  | formula_and AND formula_atom
      { fix_formula $startpos $endpos (Fixable.Formula.And ($1, $3)) }
  | formula_atom
      { $1 }

formula_atom:
  | TOP
      { fix_formula $startpos $endpos Fixable.Formula.Top }

  | BOTTOM
      { fix_formula $startpos $endpos Fixable.Formula.Bottom }

  | FORALL LPAREN LIDENT COLON ty RPAREN DOT formula
      { fix_formula $startpos $endpos (Fixable.Formula.Forall ($3, $5, $8)) }

  | EXISTS LPAREN LIDENT COLON ty RPAREN DOT formula
      { fix_formula $startpos $endpos (Fixable.Formula.Exists ($3, $5, $8)) }

  | term EQ term
      { fix_formula $startpos $endpos (Fixable.Formula.Eq ($1, $3)) }

  | LIDENT LPAREN term_list_opt RPAREN
      { fix_formula $startpos $endpos (Fixable.Formula.Pred ($1, $3)) }

  | UIDENT LPAREN term_list_opt RPAREN
      { fix_formula $startpos $endpos (Fixable.Formula.Pred ($1, $3)) }

  | LPAREN formula RPAREN
      { $2 }

term:
  | LIDENT
      { fix_term $startpos $endpos (Fixable.Term.Var $1) }

  | UIDENT
      { fix_term $startpos $endpos (Fixable.Term.Constr ($1, [])) }

  | UIDENT LPAREN term_list_opt RPAREN
      { fix_term $startpos $endpos (Fixable.Term.Constr ($1, $3)) }

  | LIDENT LPAREN term_list_opt RPAREN
      { fix_term $startpos $endpos (Fixable.Term.App ($1, $3)) }

  | LPAREN term RPAREN
      { $2 }

term_list_opt:
  |
      { [] }
  | term_list
      { $1 }

term_list:
  | term
      { [$1] }
  | term COMMA term_list
      { $1 :: $3 }
proof:
  | proof_app
      { $1 }

proof_app:
  | proof_atom
      { $1 }
  | proof_app proof_atom %prec APP
      { fix_proof $startpos $endpos (Fixable.ProofTerm.App ($1, $2)) }

proof_atom:
  | LIDENT
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Var $1) }

  | TRIV
      { fix_proof $startpos $endpos Fixable.ProofTerm.Triv }

  | ABORT proof_atom COLON formula
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Abort ($2, $4)) }

  | LPAREN proof COMMA proof RPAREN
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Pair ($2, $4)) }

  | FST proof_atom
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Fst $2) }

  | SND proof_atom
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Snd $2) }

  | LEFT proof_atom COLON formula
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Left ($2, $4)) }

  | RIGHT formula proof_atom
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Right ($2, $3)) }

| MATCH proof WITH
    BAR LEFT LIDENT DARROW proof
    BAR RIGHT LIDENT DARROW proof
    {
      fix_proof $startpos $endpos
        (Fixable.ProofTerm.Case ($2, ($6, $8), ($11, $13)))
    }



  | FUN LPAREN LIDENT COLON formula RPAREN DARROW proof
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Lam ($3, $5, $8)) }

  | GEN LPAREN LIDENT COLON ty RPAREN DARROW proof
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Gen ($3, $5, $8)) }

  | proof_atom LBRACK term RBRACK
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Inst ($1, $3)) }

  | PACK LBRACK ty COMMA term COMMA proof RBRACK
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Pack ($3, $5, $7)) }

  | UNPACK LPAREN term COMMA LIDENT COMMA LIDENT COMMA proof RPAREN
      { fix_proof $startpos $endpos (Fixable.ProofTerm.Unpack ($3, $5, $7, $9)) }

  | LPAREN proof RPAREN
      { $2 }


