{
open Parser

exception Error of string

let keyword_or_ident = function
  | "type"    -> TYPE
  | "fundecl" -> FUNDECL
  | "pred"    -> PREDDECL
  | "axiom"   -> AXIOM
  | "theorem" -> THEOREM
  | "check"   -> CHECK

  | "forall"  -> FORALL
  | "exists"  -> EXISTS
  | "fun"     -> FUN
  | "gen"     -> GEN
  | "match"   -> MATCH
  | "with"    -> WITH
  | "left"    -> LEFT
  | "right"   -> RIGHT
  | "fst"     -> FST
  | "snd"     -> SND
  | "triv"    -> TRIV
  | "abort"   -> ABORT
  | "pack"    -> PACK
  | "unpack"  -> UNPACK
  | "Top"     -> TOP
  | "Bottom"  -> BOTTOM
  | "of"      -> OF

  | s when String.length s > 0 && s.[0] = '\'' ->
      TVAR s

  | s when Char.uppercase_ascii s.[0] = s.[0] ->
      UIDENT s

  | s ->
      LIDENT s
}

let blank = [' ' '\t' '\r']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']
let ident_continue = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let lident = lower ident_continue*
let uident = upper ident_continue*
let tvar = '\'' ['a'-'z' 'A'-'Z' '_'] ident_continue*

rule token = parse
  | blank+                { token lexbuf }
  | '\n'                  { Lexing.new_line lexbuf; token lexbuf }

  | "(*"                  { comment 1 lexbuf }

  | "=>"                  { DARROW }
  | "->"                  { ARROW }
  | "/\\"                 { AND }
  | "\\/"                 { OR }

  | ':'                   { COLON }
  | '.'                   { DOT }
  | ','                   { COMMA }
  | '|'                   { BAR }
  | '='                   { EQ }
  | '*'                   { STAR }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '['                   { LBRACK }
  | ']'                   { RBRACK }

  | tvar as s             { TVAR s }
  | lident as s           { keyword_or_ident s }
  | uident as s           { keyword_or_ident s }

  | eof                   { EOF }

  | _ as c                {
      let pos = lexbuf.lex_curr_p in
      let msg =
        Printf.sprintf
          "caractère inattendu '%c' à la ligne %d, colonne %d"
          c
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol)
      in
      raise (Error msg)
    }

and comment depth = parse
  | "(*"                  { comment (depth + 1) lexbuf }
  | "*)"                  {
      if depth = 1 then token lexbuf
      else comment (depth - 1) lexbuf
    }
  | '\n'                  { Lexing.new_line lexbuf; comment depth lexbuf }
  | eof                   { raise (Error "commentaire non terminé") }
  | _                     { comment depth lexbuf }
