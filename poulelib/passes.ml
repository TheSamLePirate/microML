open LibCore.Passes

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let p = Parser.main Lexer.token lexbuf in
    close_in ic;
    p
  with e ->
    close_in_noerr ic;
    raise e

let pipeline options filename =
  parse_file filename
  >|?> (handle_option "parsing" options
          (print_pass Types.ParsedProg.pp))
