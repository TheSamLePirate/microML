
open Miniml

let () =
  let filename = Sys.argv.(1) in
  let ast = Passes.parse_file filename in
  let ast_e = Passes.bindings ast in
  let value = Passes.EvalI.compile ast_e Passes.EvalI.empty_env in
  Format.printf "%a@\n" Passes.EvalI.pp value
