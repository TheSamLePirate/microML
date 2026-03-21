module type Applicative = sig
  type 'a t
  val ret : 'a -> 'a t
  (* apply *)
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
end
module type Monade = sig
  include Applicative
  (* bind *)
  val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
end

module FoldMap : functor (Acc : sig type t end) -> Monade with type 'a t = Acc.t -> Acc.t * 'a
module Map : Monade with type 'a t = 'a
module Fold : functor (Acc : sig type t end) -> Applicative with type 'a t = Acc.t -> Acc.t
module Accumule : functor (Acc : sig type t val merge : t -> t -> t val zero : t end) -> Applicative with type 'a t = Acc.t * 'a

module ListApp : functor (A : Applicative) ->
sig
  val foldmap_snd : ('a -> 'b A.t) -> 'c * 'a -> ('c * 'b) A.t
  val fold_left_map : ('a -> 'b A.t) -> 'a list -> 'b list A.t
  val fold_left_map_couple : ('a -> 'b A.t) -> ('c -> 'd A.t) -> ('a * 'c) list -> ('b * 'd) list A.t
  val fold_left_map_snd : ('a -> 'b A.t) -> ('c * 'a) list -> ('c * 'b) list A.t
end
