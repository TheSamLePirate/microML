open Miniml

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
  ignore (Passes.pipeline filename)
