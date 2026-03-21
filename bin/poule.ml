
let file = ref None
let debug = ref []
let specs = [
    ("--debug", Arg.String (fun s -> debug := s::!debug), "add debug (args must be parsing, bindings, fields, eval or backend)");
  ]
let anon s = match !file with
  | None -> file := Some s
  | Some _ -> raise (Arg.Bad "only one input file allowed")

let options name = List.mem name !debug

let () =
  Arg.parse specs anon "usage: poule <file>";
  let filename =
    match !file with
    | Some f -> f
    | None -> raise (Arg.Bad "missing input file") in
  ignore (Poulelib.Passes.pipeline options filename)
