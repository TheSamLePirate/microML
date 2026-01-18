%{
(* Adapte ces opens à ton projet *)
open Ast
open Ast.Expr

let loc (sp, ep) = { Ast.Loc.startp = sp; Ast.Loc.endp = ep }
let p loc x = Fixed.fix_u x loc
let e loc x = Fixed.fix x loc
let ty loc x = Ty.Fixed.fix x loc

let mk_lambdas loc params body=
  List.fold_right (fun x acc -> e loc (Fixable.Lambda (x, acc))) params body

%}

(* --- Tokens --- *)
%token <int> INT
%token <bool> BOOL
%token <char> CHAR

%token TYPE OF
%token <string> TYVAR   /* ex: "'a", "'b" */

%token <string> LIDENT   /* variables, labels record */
%token <string> UIDENT   /* constructeurs */

%token LET REC IN FUN MATCH WITH
%token IF THEN ELSE
%token LPAREN RPAREN LBRACE RBRACE
%token COMMA SEMI COLON
%token BAR ARROW
%token DOT
%token EQ

%token PLUS MINUS STAR SLASH
%token ANDAND OROR
%token LT LE GT GE NEQ
%token NOT

%token EOF

%type < (Ast.Loc.t, Ast.Loc.t, string, string, string, string) Ast.TopLvl.decl list> main
%start main

(* --- Precedences --- *)
%right ARROW
%right IN
%right EQ
%left OROR
%left ANDAND
%nonassoc LT LE GT GE NEQ
%left PLUS MINUS
%left STAR SLASH
%right NOT
%left APP
%nonassoc THEN
%nonassoc ELSE

%%

main:
| decl*EOF { $1 }


decl:
| type_decl { $1 }
| let_decl  { $1 }
| letrec_decl { $1 }

let_decl:
| LET b=LIDENT EQ ex=expr
  { Ast.TopLvl.Let (b, ex) }

letrec_decl:
| LET REC b=LIDENT EQ ex=expr
  { Ast.TopLvl.LetRec (b, ex) }

/* type ('a,'b) t = ...  (ou sans params) */
type_decl:
| TYPE params=type_params name=LIDENT EQ rhs=ty_expr
  {
    Ast.TopLvl.TypeDecl (params, name, rhs)
  }

/* ---------- Paramètres de type : ('a,'b,...) ou vide ---------- */

type_params:
|                           { [] }
| tv=TYVAR                  { [tv] }
| LPAREN tvs=tyvars RPAREN  { tvs }

tyvars:
| tv=TYVAR                  { [tv] }
| tv=TYVAR COMMA tvs=tyvars { tv :: tvs }

/* ---------- Expressions de types ---------- */
/* priorité : application/atom > tuple > arrow */

ty_expr:
| a=ty_tuple ARROW b=ty_expr
    { ty (loc $sloc) (Ty.Arrow (a, b)) }
| a=ty_tuple
    { a }

ty_tuple:
| a=ty_app STAR bs=ty_tuple_tail
    { ty (loc $sloc) (Ty.Tuple (a :: bs)) }
| a=ty_app
    { a }

ty_tuple_tail:
| b=ty_app STAR bs=ty_tuple_tail { b :: bs }
| b=ty_app                       { [b] }

/* application : t1 t2 ... T  (params puis nom) */
ty_app:
| a=ty_atom args=ty_args name=ty_name
    { ty (loc $sloc) (Ty.Named (a :: args, name)) }
| name=ty_name
    { ty (loc $sloc) (Ty.Named ([], name)) }
| a=ty_atom
    { a }

ty_args:
| a=ty_atom args=ty_args { a :: args }
|                         { [] }

ty_name:
| n=LIDENT { n }

/* ---------- Atomes de type ---------- */

ty_atom:
| LPAREN t=ty_expr RPAREN { t }

| LIDENT
    {
      match $1 with
      | "int"  -> ty (loc $sloc) Ty.Integer
      | "char" -> ty (loc $sloc) Ty.Char
      | "bool" -> ty (loc $sloc) Ty.Bool
      | _      -> ty (loc $sloc) (Ty.Named ([], $1))
    }

| tv=TYVAR
    { ty (loc $sloc) (Ty.Var tv) }

/* record type: { l1 : t1; l2 : t2; ... } */
| LBRACE fs=ty_fields RBRACE
    { ty (loc $sloc) (Ty.Record fs) }

/* sum type: C1 of t | C2 of t | C3  (C3 sans arg = of unit → à toi de décider) */
| cs=ty_constructors
    { ty (loc $sloc) (Ty.Sum cs) }

/* ---------- Champs record ---------- */

ty_fields:
|                                   { [] }
| f=ty_field                        { [f] }
| f=ty_field SEMI fs=ty_fields      { f :: fs }

ty_field:
| lbl=LIDENT COLON t=ty_expr { (lbl, t) }

/* ---------- Constructeurs sum ---------- */

ty_constructors:
| c=ty_constructor                  { [c] }
| c=ty_constructor BAR cs=ty_constructors { c :: cs }

ty_constructor:
| c=UIDENT OF t=ty_expr { (c, t) }
| c=UIDENT
    {
      (* si tu veux autoriser un constructeur sans argument,
         choisis ici une convention. Ex: unit = Tuple [] si tu l’acceptes. *)
      (c, ty (loc $sloc) (Ty.Tuple []))
    }



(* -------- Patterns -------- *)
pattern:
| p = p_app { p }

p_app:
| c = UIDENT; a = p_atom
    { p (loc ($startpos, $endpos)) (Pattern.Construct (c, a)) }
| c = UIDENT
    { p (loc ($startpos, $endpos)) (Pattern.Construct (c, p (loc ($startpos, $endpos)) (Pattern.Tuple []))) }  /* constructeur nullaire */
| a = p_atom
    { a }

p_atom:
| x = LIDENT
    { p (loc ($startpos, $endpos)) (Pattern.Var x) }
| LPAREN; ps = separated_nonempty_list(COMMA, pattern); RPAREN
    {
      match ps with
      | [p1] -> p1
      | _ -> p (loc ($startpos, $endpos)) (Pattern.Tuple ps)
    }

(* -------- Expressions -------- *)

expr:
| LET; x=LIDENT; params=list(LIDENT); EQ; e1=expr; IN; e2=expr
    { e (loc ($startpos, $endpos)) (Fixable.LetIn (x, mk_lambdas (loc ($startpos, $endpos)) params e1, e2)) }

| LET; REC; f=LIDENT; params=nonempty_list(LIDENT); EQ; e1=expr; IN; e2=expr
    { e (loc ($startpos, $endpos)) (Fixable.LetRecIn (f, mk_lambdas (loc ($startpos, $endpos)) params e1, e2)) }

| LET; REC; f=LIDENT; EQ; e1=expr; IN; e2=expr
    { e (loc ($startpos, $endpos)) (Fixable.LetRecIn (f, e1, e2)) }

| LET; x=LIDENT; EQ; e1=expr; IN; e2=expr
    { e (loc ($startpos, $endpos)) (Fixable.LetIn (x, e1, e2)) }

| FUN; x = LIDENT; ARROW; body = expr
    { e (loc ($startpos, $endpos)) (Fixable.Lambda (x, body)) }
| MATCH; scrut = expr; WITH; cases = match_cases
    { e (loc ($startpos, $endpos)) (Fixable.PatternMatch (scrut, cases)) }
| IF; c = expr; THEN; t = expr; ELSE; e0 = expr
    { e (loc ($startpos, $endpos)) (Fixable.IfThenElse (c, t, e0)) }
| e_binop
    { $1 }

match_cases:
| cs = nonempty_list(match_case) { cs }

match_case:
| BAR?; p0 = pattern; ARROW; e0 = expr
    { (p0, e0) }

(* Binops / unops / application / atoms *)

e_binop:
| a = e_binop; op = binop; b = e_binop %prec EQ
    { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, op, b)) }
| e_or { $1 }

e_or:
| a = e_or; OROR; b = e_and
    { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, Or, b)) }
| e_and { $1 }

e_and:
| a = e_and; ANDAND; b = e_cmp
    { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, And, b)) }
| e_cmp { $1 }

e_cmp:
| a = e_add; LT;  b = e_add { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, Lt, b)) }
| a = e_add; LE;  b = e_add { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, Le, b)) }
| a = e_add; GT;  b = e_add { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, Gt, b)) }
| a = e_add; GE;  b = e_add { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, Ge, b)) }
| a = e_add; NEQ; b = e_add { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, Neq, b)) }
| a = e_add { a }

e_add:
| a = e_add; PLUS;  b = e_mul { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, Add, b)) }
| a = e_add; MINUS; b = e_mul { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, Sub, b)) }
| e_mul { $1 }

e_mul:
| a = e_mul; STAR;  b = e_app { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, Mul, b)) }
| a = e_mul; SLASH; b = e_app { e (loc ($startpos, $endpos)) (Fixable.BinOp (a, Div, b)) }
| e_app { $1 }

e_arg:
| e_post { $1 }   /* pas e_unary ici */

e_app:
| f = e_app; x = e_post %prec APP
    { e (loc ($startpos, $endpos)) (Fixable.Apply (f, x)) }
| a = e_unary
    { a }

e_post:
| base = e_atom; sels = list(selection)
    {
      List.fold_left
        (fun acc sel ->
          match sel with
          | `Field lab ->
              e (loc ($startpos, $endpos)) (Fixable.AccessRecord (acc, lab)))
        base sels
    }

selection:
| DOT; lab = LIDENT { `Field lab }

e_unary:
| NOT; a = e_unary
    { e (loc ($startpos, $endpos)) (Fixable.UnOp (Not, a)) }
| MINUS; a = e_unary
    { e (loc ($startpos, $endpos)) (Fixable.UnOp (Neg, a)) }
| e_atom
    { $1 }

e_atom:
| n = INT
    { e (loc ($startpos, $endpos)) (Fixable.Integer n) }
| b = BOOL
    { e (loc ($startpos, $endpos)) (Fixable.Bool b) }
| c = CHAR
    { e (loc ($startpos, $endpos)) (Fixable.Char c) }
| x = LIDENT
    { e (loc ($startpos, $endpos)) (Fixable.Var x) }
| ctor = UIDENT; arg = e_atom
    { e (loc ($startpos, $endpos)) (Fixable.Construct (ctor, arg)) }
| ctor = UIDENT
    { e (loc ($startpos, $endpos)) (Fixable.Construct (ctor, e (loc ($startpos, $endpos)) (Fixable.Tuple []))) } /* Nil */
| tuple_expr
    { $1 }
| record_expr
    { $1 }
| LPAREN; e0 = expr; RPAREN
    { e0 }

tuple_expr:
| LPAREN; es = separated_nonempty_list(COMMA, expr); RPAREN
    {
      match es with
      | [e1] -> e1
      | _ -> e (loc ($startpos, $endpos)) (Fixable.Tuple es)
    }

record_expr:
| LBRACE; fields = separated_list(SEMI, record_field); RBRACE
    { e (loc ($startpos, $endpos)) (Fixable.Record fields) }

record_field:
| lab = LIDENT; EQ; v = expr
    { (lab, v) }

binop:
| PLUS  { Add }
| MINUS { Sub }
| STAR  { Mul }
| SLASH { Div }
| ANDAND { And }
| OROR   { Or }
| LT { Lt } | LE { Le } | GT { Gt } | GE { Ge } | NEQ { Neq } | EQ { Eq }
;
