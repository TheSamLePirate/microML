open Miniml

let file = ref None
let output_file = ref "a.out"
let runtime_file = ref "_build/default/runtime/libruntime.a"
let runtime_header_dir = ref "_build/default/runtime/include"
let specs = [ ("-o", Arg.String (fun s -> output_file := s), "output file");
              ("--runtime", Arg.String (fun s -> runtime_file := s), "runtime file");
              ("-I", Arg.String (fun s -> runtime_header_dir := s), "runtime header directory");
            ]
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
  ignore (Passes.pipeline_llvm filename !output_file !runtime_file !runtime_header_dir)
