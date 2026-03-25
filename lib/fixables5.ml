open T

module type Fixable5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> 'd Pp.printer -> 'e Pp.printer -> ('a, 'b, 'c, 'd, 'e) t Pp.printer
  module Make : functor (F:Applicatives.Applicative) -> sig
    val foldmap : ('a -> 'ra F.t) -> ('b -> 'rb F.t) -> ('c -> 'rc F.t) -> ('d -> 'rd F.t) -> ('e -> 're F.t) -> ('a, 'b, 'c, 'd, 'e) t -> ('ra, 'rb, 'rc, 'rd, 're) t F.t
  end
end

module type Alignable5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val align : ('a1, 'b1, 'c1, 'd1, 'e1) t -> ('a2, 'b2, 'c2, 'd2, 'e2) t -> ( ('a1 * 'a2), ('b1 * 'b2), ('c1 * 'c2), ('d1 * 'd2), ('e1 * 'e2)) t option
end

module type Comparable5 = sig
  include Alignable5
  val tag : ('a, 'b, 'c, 'd, 'e) t -> int
end

module type SigSurface5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val fold : ('a, 'acc) fold -> ('b, 'acc) fold -> ('c, 'acc) fold -> ('d, 'acc) fold -> ('e, 'acc) fold -> (('a, 'b, 'c, 'd, 'e) t, 'acc) fold
  val map : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('c1 -> 'c2) -> ('d1 -> 'd2) -> ('e1 -> 'e2) ->  ('a1, 'b1, 'c1, 'd1, 'e1) t -> ('a2, 'b2, 'c2, 'd2, 'e2) t
  val foldmap : ('a, 'acc, 'a2) fun_foldmap -> ('b, 'acc, 'b2) fun_foldmap -> ('c, 'acc, 'c2) fun_foldmap -> ('d, 'acc, 'd2) fun_foldmap -> ('e, 'acc, 'e2) fun_foldmap -> (('a, 'b, 'c, 'd, 'e) t, 'acc, ('a2, 'b2, 'c2, 'd2, 'e2) t) fun_foldmap
end
