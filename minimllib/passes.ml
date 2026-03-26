open LibCore.Ident
open LibCore.Passes
module Pp = LibCore.Pp

module Bindings = Bindings.Make(StringBindings)(IntBindings)
module Fields = Fields.Make(StringFields)(IntFields)
module Ty = Typer.Make(IntBindings)(StringBindings)(StringBindings)(IntTypesBindings)(StringFields)
module EvalI = Eval.Make(IntBindings)(IntFields)
module Backend = AstBackend.Make(IntBindings)(IntBindings)
module CPrinter = BackendCPrinter.Make(IntBindings)
module InlineB = InlineBackend.Make(IntBindings)

let parse_file filename =
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = filename };
  let ast =
    try Parser.main Lexer.token lb
    with
    | Parser.Error ->
        let p = lb.lex_curr_p in
        let tok = Lexing.lexeme lb in
        failwith
          (Printf.sprintf "parse error at %s:%d:%d near %S"
             p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) tok)
  in
  close_in ic;
  ast


let dump ic =
  try
    while true do
      let line = input_line ic in
      print_endline line
    done
  with End_of_file -> ()

let clang_print f out =
  Format.fprintf f "#include <assert.h>@\n#include <rt.h>@\n@\n%a@\nint main(){@\n@[<v2>  runtime_init();@\n%a@\nreturn 0;@]@\n}@\n"
    (Pp.list (Pp.sep "@\n") (fun f i -> i f ())) out.CPrinter.functions
    (Pp.list (Pp.sep "@\n") (fun f i -> i f ())) (List.rev out.CPrinter.main)

let clang_pass output_file runtime_file runtime_header_dir =
  final_pass (fun prog ->
      let argv =  [| "clang"; "-v"; "-x"; "c"; "-"; "-x"; "none"; "-O0"; "-g"; "-I"; runtime_header_dir; "-o"; output_file;  runtime_file |] in
      let stdin_r, clang_stdin = Unix.pipe () in
      (* Empêche clang d’hériter de stdin_w (sinon EOF peut ne jamais arriver) *)
      Unix.set_close_on_exec clang_stdin;
      let pid = Unix.create_process "clang" argv stdin_r  Unix.stdout  Unix.stderr in
      Unix.close stdin_r;
      let oc = Unix.out_channel_of_descr clang_stdin in
      let fmt = Format.formatter_of_out_channel oc in
      ignore (clang_print fmt prog);
      Format.pp_print_flush fmt ();
      Format.pp_print_flush fmt ();
      flush oc;
      close_out oc;
      Printf.printf "OK\n";
      Format.pp_print_flush Format.std_formatter ();
      match snd (Unix.waitpid [] pid) with
      | Unix.WEXITED 0 -> ()
      | _ -> failwith "clang failed")

let pipeline_base options =
  (handle_option "parsing" options
         (print_pass (Ast.TopLvl.pp StringFields.pp StringBindings.pp StringBindings.pp StringBindings.pp)))
  ++> Bindings.passe
  ++> (handle_option "bindings" options
         (print_pass (Bindings.debug StringFields.pp StringBindings.pp StringBindings.pp)))
  ++> Ty.passe
  ++> (handle_option "typer" options (print_pass Ty.debug))
(*
(fun (env, ast) ->
    if options "typer_env" then Format.printf "typer env : @[<v>%a@]@\n" Ty.ppenv env;
    ast)
*)
  ++> Fields.passe
  ++> (handle_option "fields" options (print_pass (Fields.debug IntBindings.pp IntBindings.pp StringBindings.pp )))
  ++> Lazy.passe
  ++> (handle_option "lazy" options (print_pass (Lazy.debug IntFields.pp IntBindings.pp IntBindings.pp StringBindings.pp)))

let pipeline_eval options =
  pipeline_base options
  ++> EvalI.passe
  ++> (handle_option "eval" options (print_pass EvalI.pp))

let pipeline_llvm options output_file runtime_file runtime_header_dir =
  pipeline_base options
  ++> Backend.passe
  ++> (handle_option "backend" options (print_pass Backend.debug))
  ++> InlineB.passe
  ++> (handle_option "inline backend" options (print_pass InlineB.debug))
  ++> CPrinter.passe
  ++> final_pass (clang_print Format.std_formatter)
  ++> clang_pass output_file runtime_file runtime_header_dir
