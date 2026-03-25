
module FixableConvert3to5 (F : Fixables3.Fixable3) : Fixables5.Fixable5 with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c) F.t = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c) F.t
  let pp pp_a pp_b pp_c _pp_d _pp_e = F.pp pp_a pp_b pp_c
  module Make (A:Applicatives.Applicative) = struct
    module M = F.Make(A)
    let foldmap mapa mapb mapc _mapd _mape itm = M.foldmap mapa mapb mapc itm
  end
end
module AlignableConvert3to5 (F : Fixables3.Alignable3) : Fixables5.Alignable5 with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c) F.t = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c) F.t
  let align t1 t2 = F.align t1 t2
end
module MakeSurface3 (F : Fixables3.Fixable3) : Fixables3.SigSurface3 with type ('a, 'b, 'c) t = ('a, 'b, 'c) F.t = struct
  module Surface5 = Fix5.MakeSurface(FixableConvert3to5(F))
  type ('a, 'b, 'c) t = ('a, 'b, 'c) F.t
  let fold fa fb fc x acc = Surface5.fold fa fb fc Aux.passacc Aux.passacc x acc
  let map fa fb fc x = Surface5.map fa fb fc Fun.id Fun.id x
  let foldmap fa fb fc x acc = Surface5.foldmap fa fb fc Aux.idacc Aux.idacc x acc
end

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

module MakeSurface4 (F : Fixables4.Fixable4) : Fixables4.SigSurface4 with type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) F.t = struct
  type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) F.t
  module Surface5 = Fix5.MakeSurface(FixableConvert4to5(F))
  let fold fa fb fc fd x acc = Surface5.fold fa fb fc fd Aux.passacc x acc
  let map fa fb fc fd x = Surface5.map fa fb fc fd Fun.id x
  let foldmap fa fb fc fd x acc = Surface5.foldmap fa fb fc fd Aux.idacc x acc
end

module Fix3 (F : Fixables3.Fixable3) : Fixables3.Fixed3 with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) F.t = struct
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

  module Surface = MakeSurface3(F)

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

module Fix4 (F : Fixables4.Fixable4) = struct
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

  module Surface = MakeSurface4(F)

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

(* Quand un type récursif inclue un autre type récursif et qu'ils partagent deux variables de polymorphisme *)
module FixMut  (F : Fixables4.Fixable4) (G : Fixables3.Fixable3) = struct

  type ('deco, 'a, 'b) t = F of 'deco * (('deco, 'a, 'b) t, ('deco, 'a, 'b) u, 'a, 'b) F.t
  and ('deco, 'a, 'b) u = G of 'deco *  (('deco, 'a, 'b) u, 'a, 'b) G.t
  (* TODO faire en sorte que u soit un Fix3, les types ne sont pas mutuellement récursifs donc ça doit passer *)

  let decoration_u (G (d, _)) = d
  let decoration (F (d, _)) = d
  let fix_u a d = G (d, a)
  let fix a d = F (d, a)
  let unfix_u (G (_, a)) = a
  let unfix (F (_, a)) = a
  let wrap_acc f (F (deco, x)) acc =
    let acc, x = f x acc in acc, F (deco, x)

  module SurfaceF = MakeSurface4(F)
  module SurfaceG = MakeSurface3(G)

  module Deep = struct
    let rec mapfg_u f g (G (d, x)) = G (d, SurfaceG.map (mapfg_u f g) f g x)
    let rec mapfg f g (F (d, x)) = F (d, SurfaceF.map (mapfg f g) (mapfg_u f g) f g x)
    let rec reduce_u f (G (_, x)) = f (SurfaceG.map (reduce_u f) Fun.id Fun.id x)
    let rec reduce f g (F (_, x)) = f (SurfaceF.map (reduce f g) (reduce_u g) Fun.id Fun.id x)

    let rec reduce_u_d f (G (d, x)) = f d (SurfaceG.map (reduce_u_d f) Fun.id Fun.id x)
    let rec reduce_d f g (F (d, x)) = f d (SurfaceF.map (reduce_d f g) (reduce_u_d g) Fun.id Fun.id x)


    let rec reduce_de f (F (d, x)) = f d (SurfaceF.map (reduce_de f) Fun.id Fun.id Fun.id x)


    let rec foldmapfg_u f g (G (d, x)) acc =
      let acc, x = SurfaceG.foldmap (foldmapfg_u f g) f g x acc in
      acc, G( d, x)
    let rec foldmapfg f g (F (d, x)) acc =
      let acc, x = SurfaceF.foldmap (foldmapfg f g) (foldmapfg_u f g) f g x acc in
      acc, F( d, x)


    let rec map f (F (d, x)) = F( d, f (SurfaceF.map (map f) Fun.id Fun.id Fun.id x))
    (* map des décorations *)
    let rec decorate_u f (G (d, x)) =
      let x = SurfaceG.map (decorate_u f) Fun.id Fun.id x in
      G (f d x, x)
    let rec decorate f g (F (d, x)) =
      let x = SurfaceF.map (decorate f g) (decorate_u g) Fun.id Fun.id x in
      F (f d x, x)

    (* foldmap des décorations *)
    let rec decorate_foldmap_u f (G (d, x)) acc =
      let acc, x = SurfaceG.foldmap (decorate_foldmap_u f) Aux.idacc Aux.idacc x acc in
      let acc, d = f d x acc in
      acc, G (d, x)
    let rec decorate_foldmap f g (F (d, x)) acc =
      let acc, x = SurfaceF.foldmap (decorate_foldmap f g) (decorate_foldmap_u g) Aux.idacc Aux.idacc x acc in
      let acc, d = f d x acc in
      acc, F (d, x)

  end

  module Traverse = struct
    let rec decorate_foldmap_u f g h (G (d, x)) acc =
      let acc, d, x = f (decorate_foldmap_u f g h) (SurfaceG.foldmap (decorate_foldmap_u f g h) (g d) (h d)) acc d x in
      acc, G (d, x)
    let rec decorate_foldmap f g h i (F (d, x)) acc =
      let fin = decorate_foldmap f g h i in
      let gin = decorate_foldmap_u g h i in
      let acc, d, x = f fin gin (SurfaceF.foldmap fin gin (h d) (i d)) acc d x in
      acc, F (d, x)

    let wraphi f _d x acc = f x acc

    let wrapu f fin tra acc d x = let a, b = f fin tra acc x in a, d, b
    let wrap f fin gin tra acc d x = let a, b = f fin gin tra acc x in a, d, b
    let foldmap_u f g h = decorate_foldmap_u (wrapu f) (wraphi h) (wraphi g)
    let foldmap f g h i = decorate_foldmap (wrap f) (wrapu g) (wraphi h) (wraphi i)

  end

  let ppu ppb ppc = Deep.reduce_u (fun x f -> G.pp (fun f g -> g f) ppb ppc f x)
  let ppu ppb ppc f p = ppu ppb ppc p f

  let pp ppb ppc = Deep.reduce (fun x f -> F.pp (fun f g -> g f) (fun f x -> x f) ppb ppc f x)
                 (fun p f -> G.pp (fun f x -> x f) ppb ppc f p)

  let pp ppb ppc f p = pp ppb ppc p f

end
