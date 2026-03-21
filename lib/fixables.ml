
module type Fixable3 = sig
  type ('a, 'b, 'c) t
  val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> ('a, 'b, 'c) t Pp.printer
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
  val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> 'd Pp.printer -> ('a, 'b, 'c, 'd) t Pp.printer
  module Make : functor (F:Applicatives.Applicative) -> sig
    val foldmap : ('a -> 'ra F.t) -> ('b -> 'rb F.t) -> ('c -> 'rc F.t) -> ('d -> 'rd F.t) -> ('a, 'b, 'c, 'd) t -> ('ra, 'rb, 'rc, 'rd) t F.t
  end
end

module type Fixable5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> 'd Pp.printer -> 'e Pp.printer -> ('a, 'b, 'c, 'd, 'e) t Pp.printer
  module Make : functor (F:Applicatives.Applicative) -> sig
    val foldmap : ('a -> 'ra F.t) -> ('b -> 'rb F.t) -> ('c -> 'rc F.t) -> ('d -> 'rd F.t) -> ('e -> 're F.t) -> ('a, 'b, 'c, 'd, 'e) t -> ('ra, 'rb, 'rc, 'rd, 're) t F.t
  end
end

module type SigSurface3 = sig
  type ('a, 'b, 'c) t
  val fold : ('a -> 'acc -> 'acc) -> ('b -> 'acc -> 'acc) -> ('c -> 'acc -> 'acc) -> ('a, 'b, 'c) t -> 'acc -> 'acc
  val map : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('c1 -> 'c2) -> ('a1, 'b1, 'c1) t -> ('a2, 'b2, 'c2) t
  val foldmap : ('a -> 'acc -> 'acc * 'a2) -> ('b -> 'acc -> 'acc * 'b2) -> ('c -> 'acc -> 'acc *'c2) -> ('a, 'b, 'c) t -> 'acc -> 'acc * ('a2, 'b2, 'c2) t
end

module type SigSurface4 = sig
  type ('a, 'b, 'c, 'd) t
  val fold : ('a -> 'acc -> 'acc) -> ('b -> 'acc -> 'acc) -> ('c -> 'acc -> 'acc) -> ('d -> 'acc -> 'acc) -> ('a, 'b, 'c, 'd) t -> 'acc -> 'acc
  val map : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('c1 -> 'c2) -> ('d1 -> 'd2) ->  ('a1, 'b1, 'c1, 'd1) t -> ('a2, 'b2, 'c2, 'd2) t
  val foldmap : ('a -> 'acc -> 'acc * 'a2) -> ('b -> 'acc -> 'acc * 'b2) -> ('c -> 'acc -> 'acc *'c2) -> ('d -> 'acc -> 'acc *'d2) -> ('a, 'b, 'c, 'd) t -> 'acc -> 'acc * ('a2, 'b2, 'c2, 'd2) t
end

module type SigSurface5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val fold : ('a -> 'acc -> 'acc) -> ('b -> 'acc -> 'acc) -> ('c -> 'acc -> 'acc) -> ('d -> 'acc -> 'acc) -> ('e -> 'acc -> 'acc) -> ('a, 'b, 'c, 'd, 'e) t -> 'acc -> 'acc
  val map : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('c1 -> 'c2) -> ('d1 -> 'd2) -> ('e1 -> 'e2) ->  ('a1, 'b1, 'c1, 'd1, 'e1) t -> ('a2, 'b2, 'c2, 'd2, 'e2) t
  val foldmap : ('a -> 'acc -> 'acc * 'a2) -> ('b -> 'acc -> 'acc * 'b2) -> ('c -> 'acc -> 'acc *'c2) -> ('d -> 'acc -> 'acc *'d2) -> ('e -> 'acc -> 'acc *'e2) -> ('a, 'b, 'c, 'd, 'e) t -> 'acc -> 'acc * ('a2, 'b2, 'c2, 'd2, 'e2) t
end
