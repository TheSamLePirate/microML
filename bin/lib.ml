let id x = x

module Pp = struct
  let list sep pp f li =
    let rec print f = function
      | [] -> ()
      | hd::tl -> Format.fprintf f "%a%a%a" pp hd sep () print tl
    in print f li
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

module Fix3 (F : Fixable3) = struct
  type ('a, 'b) t = F of (('a, 'b) t, 'a, 'b) F.t
  let fix a = F a

  module MakeApp (A:Applicative) = struct
    open A
    module S = F.Make(A)
    let rec foldmap f g (F x) = ret (fun x -> F x) <*> S.foldmap (foldmap f g) f g x
  end

  module Surface = struct
    let fold (type acc) f g h x =
      let module Fold = F.Make(Applicatives.Fold(struct type t = acc end) ) in
      Fold.foldmap f g h x

    let map f g h x =
      let module Map = F.Make(Applicatives.Map) in
      Map.foldmap f g h x
  end

  module Deep = struct
    let rec mapfg f g (F x) = F (Surface.map (mapfg f g) f g x)
    let rec reduce f (F x) = f (Surface.map (reduce f) id id x) (* bottomup reduce *)
    let map f = reduce (fun x -> F(f x)) (* map bottomup *)
  end

  let pp ppb ppc = Deep.reduce (fun x f -> F.pp (fun f g -> g f) ppb ppc f x)
  let pp ppb ppc f p = pp ppb ppc p f
end
