module Loc = LibCore.Lib.Loc
module Ident = LibCore.Ident
module ParsedProg =
  PouleAst.Prog
    (Ident.StringBindings)
    (Ident.StringBindings)
    (Ident.StringBindings)
    (struct
       type deco = Loc.t
       type pred = string
     end)
