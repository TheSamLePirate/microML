module ParsedProg =
  PouleAst.Prog
    (Miniml.Ident.StringBindings)
    (Miniml.Ident.StringBindings)
    (Miniml.Ident.StringBindings)
    (struct
       type deco = Miniml.Lib.Loc.t
       type pred = string
     end)
