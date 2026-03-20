{
open Parser

exception Lexing_error of string

let keyword_table =
  [
    "type", TYPE;
    "pred", PRED;
    "func", FUNC;
    "axiom", AXIOM;
    "theorem", THEOREM;
    "check", CHECK;
    "forall", FORALL;
    "exists", EXISTS;
    "of", OF;
    "as", AS;
    "fun", FUN;
    "gen", GEN;
    "inst", INST;
    "case", CASE;
    "left", LEFT;
    "right", RIGHT;
    "fst", FST;
    "snd", SND;
    "triv", TRIV;
    "abort", ABORT;
    "Top", TOP;
    "Bottom", BOTTOM;
  ]

let ident_or_keyword s =
  match List.assoc_opt s keyword_table with
  | Some tok -> tok
  | None ->
      if String.length s > 0 && Char.uppercase_ascii s.[0] = s.[0]
      then UIDENT s
      else LIDENT s
}

let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let ident = (lower | upper) (lower | upper | digit | '\'' )*
let tyvar = '\'' lower (lower | digit | '_')*

rule token = parse
  | [' ' '\t']+               { token lexbuf }
  | '\n'                      { Lexing.new_line lexbuf; token lexbuf }
  | "\r\n"                    { Lexing.new_line lexbuf; token lexbuf }
  | '\r'                      { Lexing.new_line lexbuf; token lexbuf }

  | "(*"                      { comment lexbuf; token lexbuf }

  | "/\\"                     { AND }
  | "\\/"                     { OR }
  | "->"                      { ARROW }
  | "=>"                      { DARROW }

  | ':'                       { COLON }
  | ','                       { COMMA }
  | '|'                       { BAR }
  | '='                       { EQ }
  | '*'                       { STAR }

  | '('                       { LPAREN }
  | ')'                       { RPAREN }
  | '['                       { LBRACK }
  | ']'                       { RBRACK }

  | tyvar as s                { TYVAR s }
  | ident as s                { ident_or_keyword s }

  | eof                       { EOF }

  | _ as c {
      let p = lexbuf.lex_curr_p in
      let line = p.pos_lnum in
      let col = p.pos_cnum - p.pos_bol in
      raise (Lexing_error
               (Printf.sprintf "%s:%d:%d: caractère inattendu %C"
                  p.pos_fname line col c))
}

and comment = parse
  | "*)"                      { () }
  | '\n'                      { Lexing.new_line lexbuf; comment lexbuf }
  | "\r\n"                    { Lexing.new_line lexbuf; comment lexbuf }
  | eof                       { raise (Lexing_error "commentaire non terminé") }
  | _                         { comment lexbuf }
