open Miniml
open Ast
open Lib

let file = ref None
let specs = [ ("-o", Arg.String (fun s -> print_endline s), "output file") ]
let anon s =
  match !file with
  | None -> file := Some s
  | Some _ -> raise (Arg.Bad "only one input file allowed")

let () =
  Arg.parse specs anon "usage: microml <file>";
  let filename =
    match !file with
    | Some f -> f
    | None -> raise (Arg.Bad "missing input file")
  in
  let ast = Passes.parse_file filename in
  let () = Format.printf "test = %a@\n" (Expr.Fixed.pp Pp.string Pp.string) ast in
  let ast_e = Passes.bindings ast in
  let () = Format.printf "test = %a@\n" (Expr.Fixed.pp Pp.string (Pp.prefix "v" Pp.int)) ast_e in
  let value = Passes.EvalI.compile ast_e Passes.EvalI.empty_env in
  let () = Format.printf "value = %a@\n" Passes.EvalI.pp value in
  ()

