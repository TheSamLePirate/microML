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
  type ('deco, 'a, 'b) t = F of 'deco * (('deco, 'a, 'b) t, 'a, 'b) F.t
  type ('a, 'b, 'c) fixable = ('a, 'b, 'c) F.t

  let decoration (F (d, _)) = d
  let fix a d = F (d, a)
  let unfix (F (_, a)) = a
  let wrap_acc f (F (deco, x)) acc =
    let acc, x = f x acc in acc, F (deco, x)

  module MakeApp (A:Applicatives.Applicative) = struct (* C'est un traversable sur les deux autres variables, utilisable quand un objet de l'AST est inclu dans un autre AST (exemple : pattern matching dans expression aurait pu être implémenté comme ça) *)
    open A
    module S = F.Make(A)
    let rec foldmap f g (F (d, x)) = ret (fun x -> F (d, x)) <*> S.foldmap (foldmap f g) f g x
  end

  (* Walkers pour les réécritures *)

  module Surface = MakeSurface(F)

  module Deep = struct
    let rec mapfg f g (F (d, x)) = F (d, Surface.map (mapfg f g) f g x)
    let rec foldfg f g (F (_, x)) acc = Surface.fold (foldfg f g) f g x acc
    let rec reduce f (F (_, x)) = f (Surface.map (reduce f) Fun.id Fun.id x) (* bottomup reduce *)
    let rec map f (F (d, x)) = F( d, f (Surface.map (map f) Fun.id Fun.id x))

    let rec foldmapfg f g (F (d, x)) acc =
      let acc, x = Surface.foldmap (foldmapfg f g) f g x acc in
      acc, F( d, x)

    let rec foldmap f (F (d, x)) acc =
      let acc, x = Surface.foldmap (foldmap f) Aux.idacc Aux.idacc x acc in
      let acc, tofix = f acc x in
      acc, F( d, tofix)
    let rec decorate f g (F (d, x)) =
      let x = Surface.map (decorate f g) Fun.id Fun.id x in
      F (f d x, x)
  end

  module Traverse = struct
    let rec foldmap f g h (F (d, x)) acc =
      let acc, x = f (foldmap f g h) (Surface.foldmap (foldmap f g h) g h) acc x in
      acc, F (d, x)
  end

  (* pretty print *)
  let pp ppb ppc = Deep.reduce (fun x f -> F.pp (fun f g -> g f) ppb ppc f x) (* TODO on pourrait gérer du parenthésage mais en vrai on s'en branle un peu pour ce projet *)
  let pp ppb ppc f p = pp ppb ppc p f
end
