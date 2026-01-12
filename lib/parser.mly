%{
(* Adapte ces opens à ton projet *)
open Ast.Expr

type loc = { startp : Lexing.position; endp : Lexing.position }
let loc (sp, ep) = { startp = sp; endp = ep }

(* --- Hypothèse FixMut --- *)
(* Remplace FixMut.inj par ton constructeur (FixMut.fix / wrap / inj / etc.) *)
let p loc x = assert false

let e loc x = Fixed.fix x loc
%}

(* --- Tokens --- *)
%token <int> INT
%token <bool> BOOL
%token <char> CHAR

%token <string> LIDENT   /* variables, labels record */
%token <string> UIDENT   /* constructeurs */

%token LET REC IN FUN MATCH WITH
%token TRUE FALSE

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

(* --- AST concrete types --- *)
%type < (loc, string, string) Ast.Expr.Fixed.t> main
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

%%

main:
| expr EOF { $1 }

(* -------- Patterns -------- *)

pattern:
| x = LIDENT
    { p (loc ($startpos, $endpos)) (Pattern.Var x) }
| c = UIDENT; a = pattern_atom
    { p (loc ($startpos, $endpos)) (Pattern.Construct (c, a)) }
| pattern_tuple
    { $1 }
| pattern_atom
    { $1 }

pattern_atom:
| LPAREN; p0 = pattern; RPAREN { p0 }
| x = LIDENT { p (loc ($startpos, $endpos)) (Pattern.Var x) }
| c = UIDENT { p (loc ($startpos, $endpos)) (Pattern.Construct (c, p (Pattern.Tuple []))) } /* optionnel: C comme C () */
;

pattern_tuple:
| LPAREN; ps = separated_nonempty_list(COMMA, pattern); RPAREN
    {
      match ps with
      | [p1] -> p1
      | _ -> p (loc ($startpos, $endpos)) (Pattern.Tuple ps)
    }

(* -------- Expressions -------- *)

expr:
| LET; REC; x = LIDENT; EQ; e1 = expr; IN; e2 = expr
    { e (loc ($startpos, $endpos)) (Fixable.LetRecIn (x, e1, e2)) }
| LET; x = LIDENT; EQ; e1 = expr; IN; e2 = expr
    { e (loc ($startpos, $endpos)) (Fixable.LetIn (x, e1, e2)) }
| FUN; x = LIDENT; ARROW; body = expr
    { e (loc ($startpos, $endpos)) (Fixable.Lambda (x, body)) }
| MATCH; scrut = expr; WITH; cases = match_cases
    { e (loc ($startpos, $endpos)) (Fixable.PatternMatch (scrut, cases)) }
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

e_app:
| f = e_app; x = e_post %prec APP
    { e (loc ($startpos, $endpos)) (Fixable.Apply (f, x)) }
| e_post
    { $1 }

e_post:
| r = e_atom; DOT; lab = LIDENT
    { e (loc ($startpos, $endpos)) (Fixable.AccessRecord (r, lab)) }
| e_unary
    { $1 }

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
| LT { Lt } | LE { Le } | GT { Gt } | GE { Ge } | NEQ { Neq }
;
