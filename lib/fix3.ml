module FixableConvert3to5 (F : Fixables3.Fixable3) : Fixables5.Fixable5 with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c) F.t = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c) F.t
  let pp pp_a pp_b pp_c _pp_d _pp_e = F.pp pp_a pp_b pp_c
  module Make (A:Applicatives.Applicative) = struct
    module M = F.Make(A)
    let foldmap mapa mapb mapc _mapd _mape itm = M.foldmap mapa mapb mapc itm
  end
end

module MakeSurface (F : Fixables3.Fixable3) : Fixables3.SigSurface3 with type ('a, 'b, 'c) t = ('a, 'b, 'c) F.t = struct
  module Surface5 = Fix5.MakeSurface(FixableConvert3to5(F))
  type ('a, 'b, 'c) t = ('a, 'b, 'c) F.t
  let fold fa fb fc x acc = Surface5.fold fa fb fc Aux.passacc Aux.passacc x acc
  let map fa fb fc x = Surface5.map fa fb fc Fun.id Fun.id x
  let foldmap fa fb fc x acc = Surface5.foldmap fa fb fc Aux.idacc Aux.idacc x acc
end

module Fix (F : Fixables3.Fixable3) : Fixables3.Fixed3 with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) F.t = struct
 module Fixed5 = Fix5.Fix(FixableConvert3to5(F))
  type ('deco, 'a, 'b) t = ('deco, 'a, 'b, unit, unit)  Fixed5.t
  type ('a, 'b, 'c) fixable = ('a, 'b, 'c) F.t

  let decoration = Fixed5.decoration
  let fix = Fixed5.fix
  let unfix = Fixed5.unfix
  let wrap_acc = Fixed5.wrap_acc

  module MakeApp (A:Applicatives.Applicative) = struct (* C'est un traversable sur les deux autres variables, utilisable quand un objet de l'AST est inclu dans un autre AST (exemple : pattern matching dans expression aurait pu être implémenté comme ça) *)
    open A
    module S = F.Make(A)
    let rec foldmap f g x =
      let decoration = decoration x in
      ret (fun x -> fix x decoration) <*> S.foldmap (foldmap f g) f g (unfix x)
  end

  (* Walkers pour les réécritures *)

  module Surface = MakeSurface(F)
  module Deep = struct
    let mapfg f g x = Fixed5.Deep.mapfg f g Fun.id Fun.id x
    let foldfg f g x acc = Fixed5.Deep.foldfg f g Aux.passacc Aux.passacc x acc
    let reduce f x = Fixed5.Deep.reduce f x
    let map f x = Fixed5.Deep.map f x
    let foldmapfg f g x acc = Fixed5.Deep.foldmapfghi f g Aux.idacc Aux.idacc x acc
    let exists = Fixed5.Deep.exists
    let foldmap = Fixed5.Deep.foldmap
    let decorate f x = Fixed5.Deep.decorate f x
  end
  module Traverse = struct
    let foldmap f g h x acc = Fixed5.Traverse.foldmap f g h Aux.idacc Aux.idacc x acc
  end
  let pp ppb ppc f p = Fixed5.pp ppb ppc Pp.nope Pp.nope f p
end
