
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
  type ('deco, 'a, 'b, 'c) t = F of 'deco * (('deco, 'a, 'b, 'c) t, 'a, 'b, 'c) F.t

  let decoration (F (d, _)) = d
  let fix a d = F (d, a)
  let unfix (F (_, a)) = a
  let wrap_acc f (F (deco, x)) acc =
    let acc, x = f x acc in acc, F (deco, x)

  module MakeApp (A:Applicatives.Applicative) = struct
    open A
    module S = F.Make(A)
    let rec foldmap f g h (F (d, x)) = ret (fun x -> F (d, x)) <*> S.foldmap (foldmap f g h) f g h x
  end

  module Surface = MakeSurface(F)

  module Deep = struct
    let rec mapfg f g h (F (d, x)) = F (d, Surface.map (mapfg f g h) f g h x)
    let rec foldfg f g h (F (_, x)) acc = Surface.fold (foldfg f g h) f g h x acc
    let rec reduce f (F (_, x)) = f (Surface.map (reduce f) Fun.id Fun.id Fun.id x)
    let rec map f (F (d, x)) = F( d, f (Surface.map (map f) Fun.id Fun.id Fun.id x))
    let rec foldmap f (F (d, x)) acc =
      let acc, x = Surface.foldmap (foldmap f) Aux.idacc Aux.idacc Aux.idacc x acc in
      let acc, tofix = f acc x in
      acc, F( d, tofix)
    let rec foldmapfgh f g h (F (d, x)) acc =
      let acc, x = Surface.foldmap (foldmapfgh f g h) f g h x acc in
      acc, F( d, x)
    let rec exists f (F (_, x)) =
      f x || Surface.fold (fun x acc -> acc || exists f x) Aux.passacc Aux.passacc Aux.passacc x false
  end

  module Traverse = struct
    let rec foldmap f g h i (F (d, x)) acc =
      let acc, x = f (foldmap f g h i) (Surface.foldmap (foldmap f g h i) g h i) acc x in
      acc, F (d, x)
  end

  let pp ppb ppc ppd = Deep.reduce (fun x f -> F.pp (fun f g -> g f) ppb ppc ppd f x)
  let pp ppb ppc ppd f p = pp ppb ppc ppd p f
end
