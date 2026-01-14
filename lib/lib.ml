let id x = x
let idacc x acc = acc, x

module StringMap = Map.Make(String)

let zip_opt xs ys =
  let rec aux acc xs ys =
    match xs, ys with
    | [], [] -> Some (List.rev acc)
    | x::xs, y::ys -> aux ((x,y)::acc) xs ys
    | _ -> None
  in
  aux [] xs ys

module Pp = struct
  let int f i = Format.fprintf f "%d" i
  let string f s = Format.fprintf f "%S" s
  let prefix s p f x = Format.fprintf f "%s%a" s p x
  let list sep pp f li =
    let rec print f = function
      | [] -> ()
      | [hd] -> pp f hd
      | hd::((_::_2) as tl) -> Format.fprintf f "%a%a%a" pp hd sep () print tl
    in print f li
  let failwith fmt = Format.kasprintf failwith fmt

end

module type Applicative = sig
  type 'a t
  val ret : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end

module Applicatives = struct
  module FoldMap (Acc : sig type t end) = struct
    type 'a t = Acc.t -> Acc.t * 'a
    let ret x acc = acc, x
    let (<*>) f g acc =
      let acc, e1 = f acc in
      let acc, e2 = g acc in
      acc, e1 e2
    let (=<<) f t acc =
      let acc, x = t acc in
      f x acc
  end
  module Map = struct
    type 'a t = 'a
    let ret x = x
    let (<*>) f g = f g
    let (=<<) f x = f x
  end
  module Fold (Acc : sig type t end) = struct
    type 'a t = (Acc.t -> Acc.t)
    let ret _ acc = acc
    let (<*>) f g acc = g (f acc)
  end
  module Accumule (Acc : sig type t
      val merge : t -> t -> t
      val zero : t
    end) = struct
    type 'a t = Acc.t * 'a
    let ret x = Acc.zero, x
    let (<*>) (acc1, f1) (acc2, x) = Acc.merge acc1 acc2, f1 x
  end
end

module ListApp (A:Applicative) = struct
  open A
  let fold_left_map f l = List.fold_right (fun x xs -> ret List.cons <*> f x <*> xs) l (ret [])
  let fold_left_map_couple f g l = List.fold_right (fun (a, b) xs -> ret List.cons <*> (ret (fun a b -> a, b) <*> f a <*> g b ) <*> xs) l (ret [])
end

module type Fixable3 = sig
  type ('a, 'b, 'c) t
  val pp : (Format.formatter -> 'a -> unit) ->
           (Format.formatter -> 'b -> unit) ->
           (Format.formatter -> 'c -> unit) ->
           Format.formatter -> ('a, 'b, 'c) t -> unit
  module Make : functor (F:Applicative) -> sig
    val foldmap : ('a -> 'ra F.t) -> ('b -> 'rb F.t) -> ('c -> 'rc F.t) -> ('a, 'b, 'c) t -> ('ra, 'rb, 'rc) t F.t
  end
end

module type Alignable3 = sig
  type ('a, 'b, 'c) t
  val align : ('a1,'b1,'c1) t -> ('a2,'b2,'c2) t -> ( ('a1 * 'a2), ('b1 * 'b2), ('c1 * 'c2) ) t option
end

module type Fixable4 = sig
  type ('a, 'b, 'c, 'd) t
  val pp : (Format.formatter -> 'a -> unit) ->
           (Format.formatter -> 'b -> unit) ->
           (Format.formatter -> 'c -> unit) ->
           (Format.formatter -> 'd -> unit) ->
           Format.formatter -> ('a, 'b, 'c, 'd) t -> unit
  module Make : functor (F:Applicative) -> sig
    val foldmap : ('a -> 'ra F.t) -> ('b -> 'rb F.t) -> ('c -> 'rc F.t) -> ('d -> 'rd F.t) -> ('a, 'b, 'c, 'd) t -> ('ra, 'rb, 'rc, 'rd) t F.t
  end
end


module MakeSurface3 (F : Fixable3) = struct
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

module Fix3 (F : Fixable3) = struct
  type ('deco, 'a, 'b) t = F of 'deco * (('deco, 'a, 'b) t, 'a, 'b) F.t

  let decoration (F (d, _)) = d
  let fix a d = F (d, a)
  let unfix (F (_, a)) = a
  let wrap_acc f (F (deco, x)) acc =
    let acc, x = f x acc in acc, F (deco, x)

  module MakeApp (A:Applicative) = struct (* C'est un traversable sur les deux autres variables, utilisable quand un objet de l'AST est inclu dans un autre AST (exemple : pattern matching dans expression aurait pu être implémenté comme ça) *)
    open A
    module S = F.Make(A)
    let rec foldmap f g (F (d, x)) = ret (fun x -> F (d, x)) <*> S.foldmap (foldmap f g) f g x
  end

  (* Walkers pour les réécritures *)

  module Surface = MakeSurface3(F)

  module Deep = struct
    let rec mapfg f g (F (d, x)) = F (d, Surface.map (mapfg f g) f g x)
    let rec reduce f (F (_, x)) = f (Surface.map (reduce f) id id x) (* bottomup reduce *)
    let rec map f (F (d, x)) = F( d, f (Surface.map (map f) id id x))
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

(* Quand un type récursif inclue un autre type récursif et qu'ils partagent deux variables de polymorphisme *)
module FixMut  (F : Fixable4) (G : Fixable3) = struct

  type ('deco, 'a, 'b) t = F of 'deco * (('deco, 'a, 'b) t, ('deco, 'a, 'b) u, 'a, 'b) F.t
  and ('deco, 'a, 'b) u = G of 'deco *  (('deco, 'a, 'b) u, 'a, 'b) G.t

  let decoration_u (G (d, _)) = d
  let decoration (F (d, _)) = d
  let fix_u a d = G (d, a)
  let fix a d = F (d, a)
  let unfix_u (G (_, a)) = a
  let unfix (F (_, a)) = a
  let wrap_acc f (F (deco, x)) acc =
    let acc, x = f x acc in acc, F (deco, x)

  module SurfaceF = struct
    let fold (type acc) f g h i x acc =
      let module Fold = F.Make(Applicatives.Fold(struct type t = acc end) ) in
      Fold.foldmap f g h i x acc

    let map f g h i x =
      let module Map = F.Make(Applicatives.Map) in
      Map.foldmap f g h i x

    let foldmap (type acc) f g h i x acc =
      let module FoldMap = Applicatives.FoldMap(struct type t = acc end) in
      let module M = F.Make(FoldMap) in
      M.foldmap f g h i x acc
  end
  module SurfaceG = MakeSurface3(G)

  module Deep = struct
    let rec mapfg_u f g (G (d, x)) = G (d, SurfaceG.map (mapfg_u f g) f g x)
    let rec mapfg f g (F (d, x)) = F (d, SurfaceF.map (mapfg f g) (mapfg_u f g) f g x)
    let rec reduce_u f (G (_, x)) = f (SurfaceG.map (reduce_u f) id id x)
    let rec reduce f g (F (_, x)) = f (SurfaceF.map (reduce f g) (reduce_u g) id id x)
    let rec map f (F (d, x)) = F( d, f (SurfaceF.map (map f) id id id x))

    (* map des décorations *)
    let rec decorate_u f (G (d, x)) =
      let x = SurfaceG.map (decorate_u f) id id x in
      G (f d x, x)
    let rec decorate f g (F (d, x)) =
      let x = SurfaceF.map (decorate f g) (decorate_u g) id id x in
      F (f d x, x)

    (* foldmap des décorations *)
    let rec decorate_foldmap_u f (G (d, x)) acc =
      let acc, x = SurfaceG.foldmap (decorate_foldmap_u f) idacc idacc x acc in
      let acc, d = f d x acc in
      acc, G (d, x)
    let rec decorate_foldmap f g (F (d, x)) acc =
      let acc, x = SurfaceF.foldmap (decorate_foldmap f g) (decorate_foldmap_u g) idacc idacc x acc in
      let acc, d = f d x acc in
      acc, F (d, x)

  end

  module Traverse = struct
    let rec foldmap_u f g h (G (d, x)) acc =
      let acc, x = f (foldmap_u f g h) (SurfaceG.foldmap (foldmap_u f g h) g h) acc x in
      acc, G (d, x)
    let rec foldmap f g h i (F (d, x)) acc =
      let fin = foldmap f g h i in
      let gin = foldmap_u g h i in
      let acc, x = f fin gin (SurfaceF.foldmap fin gin h i) acc x in
      acc, F (d, x)
  end

  let ppu ppb ppc = Deep.reduce_u (fun x f -> G.pp (fun f g -> g f) ppb ppc f x)
  let ppu ppb ppc f p = ppu ppb ppc p f

  let pp ppb ppc = Deep.reduce (fun x f -> F.pp (fun f g -> g f) (fun f x -> x f) ppb ppc f x)
                 (fun p f -> G.pp (fun f x -> x f) ppb ppc f p)

  let pp ppb ppc f p = pp ppb ppc p f

end
