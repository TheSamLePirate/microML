open Lib

module StringBindings : Ast.Bindings with type t = string = struct
  include String
  module Map = Map.Make(String)
  module Set = Set.Make(String)
  let pp = Pp.string
end
module IntBindings : Ast.FreshableBindings with type t = int = struct
  include Int
  module Map = Map.Make(Int)
  module Set = Set.Make(Int)
  let pp f i = Format.fprintf f "v%d" i
  let init = 0
  let next n = n + 1
end
module IntTypesBindings : Ast.FreshableBindings with type t = int = struct
  include Int
  module Map = Map.Make(Int)
  module Set = Set.Make(Int)
  let pp = Ast.Ty.pp_tyvar
  let init = 0
  let next n = n + 1
end
module StringFields : Ast.Fields with type t = string = struct
  include String
  module Map = Map.Make(String)
  module Set = Set.Make(String)
  let pp = Pp.string
end
module IntFields : Ast.Fields with type t = int = struct
  include Int
  module Map = Map.Make(Int)
  module Set = Set.Make(Int)
  let pp = Pp.int
end

module Bindings = Bindings.Make(StringBindings)(IntBindings)
module Ty = Typer.Make(IntBindings)(StringBindings)(StringBindings)(IntTypesBindings)(StringFields)
module EvalI = Eval.Make(IntBindings)(StringFields)
module Backend = AstBackend.Make(IntBindings)(StringFields)(IntBindings)
module CPrinter = BackendCPrinter.Make(IntBindings)

let parse_file filename =
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = filename };
  let ast =
    try Parser.main Lexer.token lb
    with
    | Parser.Error ->
        let p = lb.lex_curr_p in
        failwith
          (Printf.sprintf "parse error at %s:%d:%d"
             p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol))
  in
  close_in ic;
  ast

let print_pass pp = {Ast.init = ();
                 Ast.process =
                   fun () value -> Format.printf "%a@\n" pp value; (), value
                }

let dump ic =
  try
    while true do
      let line = input_line ic in
      print_endline line
    done
  with End_of_file -> ()

let clang_pass (main, funs) =
  let clang_stdout, clang_stdin, clang_stderr =
    Unix.open_process_args_full
      "clang"
       [| "clang";
          "-x"; "c"; "-";
          "-x"; "none";
          "-O0"; "-g";
          "-I"; "runtime";
          "-o"; "/tmp/a.out";
          "_build/default/runtime/libruntime.a";
       |]
      (Unix.environment ())
  in
  let fmt = Format.formatter_of_out_channel clang_stdin in
  Format.fprintf fmt "#include \"rt.h\"@\n%a@\nint main(){@\n@[<v2>  %a@]@\n}@\n"
    (Pp.list (Pp.sep "@\n") (fun f i -> i f ())) funs
    (Pp.list (Pp.sep "@\n") (fun f i -> i f ())) main;
  Format.pp_print_flush fmt ();
  close_out clang_stdin;
  dump clang_stderr;
  dump clang_stdout;
  match Unix.close_process (clang_stdout, clang_stdin) with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith "clang failed"


(* TODO gérer un genre d'option, gestion des erreurs, avancer petit à petit*)
let (>*>) ast passe = List.fold_left_map passe.Ast.process passe.Ast.init ast
let (>|>) ast passe = snd (ast >*> passe)

let pipeline_base filename =
  parse_file filename
  >|> Bindings.passe
  >|> Ty.passe

let pipeline_eval filename =
  pipeline_base filename
  >|> EvalI.passe
  >|> print_pass EvalI.pp

let pipeline_llvm filename =
  pipeline_base filename
  >|> Backend.passe
  >|> print_pass Backend.debug
  >*> CPrinter.passe
  |> (fun (main, funs) ->
    Format.printf "%a@\nint main(){@\n@[<v2>  %a@]@\n}@\n"
      (Pp.list (Pp.sep "@\n") (fun f i -> i f ())) funs
      (Pp.list (Pp.sep "@\n") (fun f i -> i f ())) main;
    (main, funs)
  )
 |> clang_pass
