module AlignableConvert3to5 (F : Fixables3.Alignable3) : Fixables5.Alignable5 with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c) F.t = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c) F.t
  let align t1 t2 = F.align t1 t2
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

  module SurfaceF = Fix4.MakeSurface(F)
  module SurfaceG = Fix3.MakeSurface(G)

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
