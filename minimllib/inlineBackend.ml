module type Bindings = LibCore.Ident.Bindings

module Pp = LibCore.Pp
module Make(B:Bindings) = struct

  module M = AstBackend.Fixable.Make(LibCore.Applicatives.Map)

  let foldatm acc atm = match atm with
    | AstBackend.Fixable.Var v -> Option.value (B.Map.find_opt v acc) ~default:atm
    | _ -> atm

  let foldrhs acc rhs = M.foldmap_rhs (fun () atm -> foldatm acc atm) Fun.id () rhs

  let foldt body acc = AstBackend.Fixed.Traverse.foldmap (fun fin _tra acc e ->
     match e with
     | AstBackend.Fixable.Let (binding, AstBackend.Fixable.Atom atm, next) ->
        let atm = foldatm acc atm in
        let _, next = fin next (B.Map.add binding atm acc) in
        acc, AstBackend.Fixed.unfix next
     | AstBackend.Fixable.Let (binding, rhs, next) ->
        let _, next = fin next acc in
        let rhs = foldrhs acc rhs in
        acc, AstBackend.Fixable.Let (binding, rhs, next)
     | AstBackend.Fixable.LetIf (binding, atm, e1, e2, e3) ->
        let _, e1 = fin e1 acc
        and _, e2 = fin e2 acc
        and _, e3 = fin e3 acc
        and atm = foldatm acc atm in
        acc, AstBackend.Fixable.LetIf (binding, atm, e1, e2, e3)
     | AstBackend.Fixable.LetIfTag (binding, atm, field, e1, e2, e3) ->
        let _, e1 = fin e1 acc
        and _, e2 = fin e2 acc
        and _, e3 = fin e3 acc
        and atm = foldatm acc atm in
        acc, AstBackend.Fixable.LetIfTag (binding, atm, field, e1, e2, e3)
     | AstBackend.Fixable.Return atm -> acc, AstBackend.Fixable.Return (foldatm acc atm)
     | AstBackend.Fixable.AssertFalse -> acc, AstBackend.Fixable.AssertFalse )
                         (fun p acc -> acc, p)
                         (fun p acc -> acc, p) body acc

  let inline acc p =
    let acc, functions = List.fold_left_map (fun acc func ->
                             let acc, body = foldt func.AstBackend.body acc in
                             acc, { func with AstBackend.body=body}) acc p.AstBackend.functions in
    let acc, entry = List.fold_left_map (fun acc (name, def) ->
                         let acc, def = foldt def acc in
                         acc, (name, def)) acc p.AstBackend.entry in
    acc, {entry=entry; AstBackend.functions=functions}

  let passe = { LibCore.Passes.init = B.Map.empty; process = inline; }

  let pp_fun f func = Format.fprintf f "function %a(%a){@\n@[<v2>  %a@]@\n}"
                        B.pp func.AstBackend.id
                        (Pp.list (Pp.sep ", ") B.pp) func.AstBackend.args
                        (AstBackend.Fixed.pp B.pp B.pp) func.AstBackend.body

  let debug f prog =
     Format.fprintf f "%a@\n%a@\n"
       (Pp.list (Pp.sep "@\n") pp_fun) prog.AstBackend.functions
       (Pp.list (Pp.sep "@\n") (fun f (var, body) -> Format.fprintf f "%a=@\n@[<v2>  %a@]" B.pp var (AstBackend.Fixed.pp B.pp B.pp) body)) prog.AstBackend.entry

end
