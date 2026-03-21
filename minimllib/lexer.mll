{
open Parser

exception Lexing_error of string

let kw = function
  | "let" -> LET
  | "rec" -> REC
  | "in"  -> IN
  | "fun" -> FUN
  | "match" -> MATCH
  | "with"  -> WITH
  | "true"  -> BOOL true
  | "false" -> BOOL false
  | "if"   -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "type"  -> TYPE
  | "of"    -> OF
  | "external" -> EXTERNAL
  | _ -> raise Not_found

let char_of_escape = function
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 't' -> '\t'
  | '\\' -> '\\'
  | '\'' -> '\''
  | '0' -> '\000'
  | c -> c
}

let digit = ['0'-'9']
let lident_start = ['a'-'z' '_']
let uident_start = ['A'-'Z']
let ident_char = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let identchar = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']

let lident = lident_start ident_char*
let uident = uident_start ident_char*

(* variable de type : 'a, 'foo, 't1 *)
let tyvar = '\'' ['a'-'z' '_'] identchar*


rule token = parse
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t' '\r'] { token lexbuf }

| "(*" { comment lexbuf; token lexbuf }

| "->" { ARROW }
| "&&" { ANDAND }
| "||" { OROR }
| "<=" { LE }
| ">=" { GE }
| "<>" { NEQ }
| "<"  { LT }
| ">"  { GT }

| "="  { EQ }
| "+"  { PLUS }
| "-"  { MINUS }
| "*"  { STAR }
| "/"  { SLASH }
| "!"  { NOT }

| "(" { LPAREN }
| ")" { RPAREN }
| "{" { LBRACE }
| "}" { RBRACE }
| "," { COMMA }
| ";" { SEMI }
| ":" { COLON }
| "|" { BAR }
| "." { DOT }

| digit+ as s { INT (int_of_string s) }

| "'" (['\\'] ['n''r''t''\\''\'''0']) "'" {
    let c = char_of_escape (Lexing.lexeme_char lexbuf 2) in
    CHAR c
  }
| "'" ([^'\\'] as c) "'" { CHAR c }

| '"' ([^ '"' ]* as s) '"' { STRING s }

| tyvar as s { TYVAR s }

| lident as s {
    try kw s with Not_found -> LIDENT s
  }

| uident as s { UIDENT s }

| eof { EOF }

| _ as c {
    raise (Lexing_error (Printf.sprintf "Unexpected char: %C" c))
  }

and comment = parse
| "*)" { () }
| eof  { raise (Lexing_error "Unterminated comment") }
| _    { comment lexbuf }
