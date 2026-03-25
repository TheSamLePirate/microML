
module FixableConvert4to5 (F : Fixables4.Fixable4) : Fixables5.Fixable5 with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd) F.t = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd) F.t
  let pp pp_a pp_b pp_c pp_d _pp_e = F.pp pp_a pp_b pp_c pp_d
  module Make (A:Applicatives.Applicative) = struct
    module M = F.Make(A)
    let foldmap mapa mapb mapc mapd _mape itm = M.foldmap mapa mapb mapc mapd itm
  end
end
module AlignableConvert4to5 (F : Fixables4.Alignable4) : Fixables5.Alignable5 with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd) F.t = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd) F.t
  let align t1 t2 = F.align t1 t2
end

module MakeSurface (F : Fixables4.Fixable4) : Fixables4.SigSurface4 with type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) F.t = struct
  type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) F.t
  module Surface5 = Fix5.MakeSurface(FixableConvert4to5(F))
  let fold fa fb fc fd x acc = Surface5.fold fa fb fc fd Aux.passacc x acc
  let map fa fb fc fd x = Surface5.map fa fb fc fd Fun.id x
  let foldmap fa fb fc fd x acc = Surface5.foldmap fa fb fc fd Aux.idacc x acc
end

module Fix (F : Fixables4.Fixable4) = struct
  module Fixed5 = Fix5.Fix(FixableConvert4to5(F))
  type ('deco, 'a, 'b, 'c) t = ('deco, 'a, 'b, 'c, unit)  Fixed5.t
  type ('a, 'b, 'c, 'd) fixable = ('a, 'b, 'c, 'd) F.t
  let decoration = Fixed5.decoration
  let fix = Fixed5.fix
  let unfix = Fixed5.unfix
  let wrap_acc = Fixed5.wrap_acc
  module MakeApp (A:Applicatives.Applicative) = struct
    open A
    module S = F.Make(A)
    let rec foldmap f g h x =
      let decoration = decoration x in
      ret (fun x -> fix x decoration) <*> S.foldmap (foldmap f g h) f g h (unfix x)
  end
  module Surface = MakeSurface(F)
  module Deep = struct
    let mapfg f g h x = Fixed5.Deep.mapfg f g h Fun.id x
    let foldfg f g h x acc = Fixed5.Deep.foldfg f g h Aux.passacc x acc
    let reduce f x = Fixed5.Deep.reduce f x
    let map f x = Fixed5.Deep.map f x
    let foldmapfgh f g h x acc = Fixed5.Deep.foldmapfghi f g h Aux.idacc x acc
    let exists = Fixed5.Deep.exists
    let foldmap = Fixed5.Deep.foldmap
  end
  module Traverse = struct
    let foldmap f g h i x acc = Fixed5.Traverse.foldmap f g h i Aux.idacc x acc
  end
  let pp ppb ppc ppd f p = Fixed5.pp ppb ppc ppd Pp.nope f p
end
