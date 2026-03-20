
let file = ref None
let specs = []
let anon s = match !file with
  | None -> file := Some s
  | Some _ -> raise (Arg.Bad "only one input file allowed")

let () =
  Arg.parse specs anon "usage: poule <file>";
  let filename =
    match !file with
    | Some f -> f
    | None -> raise (Arg.Bad "missing input file") in
  Poule.Passes.pipeline_eval options filename
