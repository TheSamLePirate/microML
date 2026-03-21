
module type Fixable3 = sig
  type ('a, 'b, 'c) t
  val pp : (Format.formatter -> 'a -> unit) ->
           (Format.formatter -> 'b -> unit) ->
           (Format.formatter -> 'c -> unit) ->
           Format.formatter -> ('a, 'b, 'c) t -> unit
  module Make : functor (F:Applicatives.Applicative) -> sig
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
  module Make : functor (F:Applicatives.Applicative) -> sig
    val foldmap : ('a -> 'ra F.t) -> ('b -> 'rb F.t) -> ('c -> 'rc F.t) -> ('d -> 'rd F.t) -> ('a, 'b, 'c, 'd) t -> ('ra, 'rb, 'rc, 'rd) t F.t
  end
end

module type Fixable5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val pp : (Format.formatter -> 'a -> unit) ->
           (Format.formatter -> 'b -> unit) ->
           (Format.formatter -> 'c -> unit) ->
           (Format.formatter -> 'd -> unit) ->
           (Format.formatter -> 'e -> unit) ->
           Format.formatter -> ('a, 'b, 'c, 'd, 'e) t -> unit
  module Make : functor (F:Applicatives.Applicative) -> sig
    val foldmap : ('a -> 'ra F.t) -> ('b -> 'rb F.t) -> ('c -> 'rc F.t) -> ('d -> 'rd F.t) -> ('e -> 're F.t) -> ('a, 'b, 'c, 'd, 'e) t -> ('ra, 'rb, 'rc, 'rd, 're) t F.t
  end
end
