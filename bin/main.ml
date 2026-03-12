open Miniml

let debug = ref []
let eval = ref false
let file = ref None
let output_file = ref "a.out"
let runtime_file = ref "_build/default/runtime/libruntime.a"
let runtime_header_dir = ref "_build/default/runtime/include"
let specs = [
    ("--eval", Arg.Unit (fun () -> eval := true), "mode évaluation");
    ("--debug", Arg.String (fun s -> debug := s::!debug), "add debug (args must be parsing, bindings, fields, eval or backend)");
    ("-o", Arg.String (fun s -> output_file := s), "output file");
    ("--runtime", Arg.String (fun s -> runtime_file := s), "runtime file");
    ("-I", Arg.String (fun s -> runtime_header_dir := s), "runtime header directory");
  ]
let anon s =
  match !file with
  | None -> file := Some s
  | Some _ -> raise (Arg.Bad "only one input file allowed")

let options name = List.mem name !debug

let () =
  Arg.parse specs anon "usage: microml <file>";
  let filename =
    match !file with
    | Some f -> f
    | None -> raise (Arg.Bad "missing input file")
  in try
    if !eval then
      ignore (Passes.pipeline_eval options filename)
    else
      ignore (Passes.pipeline_llvm options filename !output_file !runtime_file !runtime_header_dir)
  with (Failure msg) as e ->
    Format.printf "%s\n\n" msg;
    raise e
