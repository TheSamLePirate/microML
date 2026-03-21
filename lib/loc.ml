type t = { startp : Lexing.position; endp : Lexing.position }
let loc s e = { startp=s; endp=e}
let pp f l =
  let sp = l.startp and ep = l.endp in
  Format.fprintf f "<%s>%d:%d-%d:%d"
    l.startp.pos_fname
    sp.pos_lnum (sp.pos_cnum - sp.pos_bol)
    ep.pos_lnum (ep.pos_cnum - ep.pos_bol)
