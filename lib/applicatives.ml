
module type Applicative = sig
  type 'a t
  val ret : 'a -> 'a t
  (* apply *)
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end
module type Monade = sig
  include Applicative
  (* bind *)
  val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
end

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

module ListApp (A:Applicative) = struct
  open A
  let foldmap_snd f (i, j) = ret (fun j -> i, j) <*> f j
  let fold_left_map f l = List.fold_right (fun x xs -> ret List.cons <*> f x <*> xs) l (ret [])
  let fold_left_map_couple f g l = List.fold_right (fun (a, b) xs -> ret List.cons <*> (ret (fun a b -> a, b) <*> f a <*> g b ) <*> xs) l (ret [])
  let fold_left_map_snd f l = fold_left_map (foldmap_snd f) l
end
