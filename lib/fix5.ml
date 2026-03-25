module MakeSurface (F : Fixables5.Fixable) : Fixables5.SigSurface with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) F.t = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) F.t
  let fold (type acc) x =
    let module Fold = F.Make(Applicatives.Fold(struct type t = acc end) ) in
    Fold.foldmap x
  let map x =
    let module Map = F.Make(Applicatives.Map) in
    Map.foldmap x
  let foldmap (type acc) x =
    let module FoldMap = Applicatives.FoldMap(struct type t = acc end) in
    let module M = F.Make(FoldMap) in
    M.foldmap x
end

module Fix (F : Fixables5.Fixable) = struct
  type ('a, 'b, 'c, 'd, 'e) fixable = ('a, 'b, 'c, 'd, 'e) F.t
  type ('deco, 'a, 'b, 'c, 'd) t = F of 'deco * (('deco, 'a, 'b, 'c, 'd) t, 'a, 'b, 'c, 'd) F.t

  let decoration (F (d, _)) = d
  let fix a d = F (d, a)
  let unfix (F (_, a)) = a
  let wrap_acc f (F (deco, x)) acc =
    let acc, x = f x acc in acc, F (deco, x)

  module MakeApp (A:Applicatives.Applicative) = struct
    open A
    module S = F.Make(A)
    let rec foldmap f g h i (F (d, x)) = ret (fun x -> F (d, x)) <*> S.foldmap (foldmap f g h i) f g h i x
  end

  module Surface = MakeSurface(F)

  module Deep = struct
    let rec mapfg f g h i (F (d, x)) = F (d, Surface.map (mapfg f g h i) f g h i x)
    let rec foldfg f g h i (F (_, x)) acc = Surface.fold (foldfg f g h i) f g h i x acc
    let rec reduce f (F (_, x)) = f (Surface.map (reduce f) Fun.id Fun.id Fun.id Fun.id x)
    let rec map f (F (d, x)) = F( d, f (Surface.map (map f) Fun.id Fun.id Fun.id Fun.id x))
    let rec foldmap f (F (d, x)) acc =
      let acc, x = Surface.foldmap (foldmap f) Aux.idacc Aux.idacc Aux.idacc Aux.idacc x acc in
      let acc, tofix = f acc x in
      acc, F( d, tofix)
    let rec foldmapfghi f g h i (F (d, x)) acc =
      let acc, x = Surface.foldmap (foldmapfghi f g h i) f g h i x acc in
      acc, F( d, x)

    let rec decorate f (F (d, x)) =
      let x = Surface.map (decorate f) Fun.id Fun.id Fun.id Fun.id x in
      F (f d x, x)

    let rec exists f (F (_, x)) =
      f x || Surface.fold (fun x acc -> acc || exists f x) Aux.passacc Aux.passacc Aux.passacc Aux.passacc x false
  end

  module Traverse = struct
    let rec foldmap f g h i j (F (d, x)) acc =
      let acc, x = f (foldmap f g h i j) (Surface.foldmap (foldmap f g h i j) g h i j) acc x in
      acc, F (d, x)
  end

  let pp ppb ppc ppd ppe = Deep.reduce (fun x f -> F.pp (fun f g -> g f) ppb ppc ppd ppe f x)
  let pp ppb ppc ppd ppe f p = pp ppb ppc ppd ppe p f
end
