open T

module type Fixable = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> 'd Pp.printer -> 'e Pp.printer -> ('a, 'b, 'c, 'd, 'e) t Pp.printer
  module Make : functor (F:Applicatives.Applicative) -> sig
    val foldmap : ('a -> 'ra F.t) -> ('b -> 'rb F.t) -> ('c -> 'rc F.t) -> ('d -> 'rd F.t) -> ('e -> 're F.t) -> ('a, 'b, 'c, 'd, 'e) t -> ('ra, 'rb, 'rc, 'rd, 're) t F.t
  end
end

module type Alignable = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val align : ('a1, 'b1, 'c1, 'd1, 'e1) t -> ('a2, 'b2, 'c2, 'd2, 'e2) t -> ( ('a1 * 'a2), ('b1 * 'b2), ('c1 * 'c2), ('d1 * 'd2), ('e1 * 'e2)) t option
end

module type Comparable = sig
  include Alignable
  val tag : ('a, 'b, 'c, 'd, 'e) t -> int
end

module type SigSurface = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val fold : ('a, 'acc) fold -> ('b, 'acc) fold -> ('c, 'acc) fold -> ('d, 'acc) fold -> ('e, 'acc) fold -> (('a, 'b, 'c, 'd, 'e) t, 'acc) fold
  val map : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('c1 -> 'c2) -> ('d1 -> 'd2) -> ('e1 -> 'e2) ->  ('a1, 'b1, 'c1, 'd1, 'e1) t -> ('a2, 'b2, 'c2, 'd2, 'e2) t
  val foldmap : ('a, 'acc, 'a2) fun_foldmap -> ('b, 'acc, 'b2) fun_foldmap -> ('c, 'acc, 'c2) fun_foldmap -> ('d, 'acc, 'd2) fun_foldmap -> ('e, 'acc, 'e2) fun_foldmap -> (('a, 'b, 'c, 'd, 'e) t, 'acc, ('a2, 'b2, 'c2, 'd2, 'e2) t) fun_foldmap
end

module type Fixed = sig
  type ('deco, 'a, 'b, 'c, 'd) t (* = F of 'deco * (('deco, 'a, 'b, 'c, 'd) t, 'a, 'b, 'c, 'd) F.t
*)
  type ('a, 'b, 'c, 'd, 'e) fixable
  val decoration : ('deco, 'b, 'c, 'd, 'e) t -> 'deco
  val fix : (('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) fixable -> 'deco -> ('deco, 'b, 'c, 'd, 'e) t
  val unfix : ('deco, 'b, 'c, 'd, 'e) t -> (('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) fixable
  val wrap_acc : ((('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) fixable -> 'f -> 'g * (('deco, 'h, 'i, 'j, 'k) t, 'h, 'i, 'j, 'k) fixable) -> ('deco, 'b, 'c, 'd, 'e) t -> 'f -> 'g * ('deco, 'h, 'i, 'j, 'k) t
  module MakeApp : functor (A : Applicatives.Applicative) -> sig
    val foldmap : ('a -> 'b A.t) -> ('c -> 'd A.t) -> ('e -> 'f A.t) -> ('g -> 'h A.t) -> ('i, 'a, 'c, 'e, 'g) t -> ('i, 'b, 'd, 'f, 'h) t A.t
  end
  module Surface : SigSurface with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) fixable
  module Deep :
  sig
    val mapfg : ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'h) -> ('deco, 'a, 'c, 'e, 'g) t -> ('deco, 'b, 'd, 'f, 'h) t
    val foldfg : ('a, 'acc) fold -> ('c, 'acc) fold -> ('d, 'acc) fold -> ('e, 'acc) fold -> (('deco, 'a, 'c, 'd, 'e) t, 'acc) fold
    val reduce : (('acc, 'b, 'c, 'd, 'e) fixable -> 'acc) -> ('deco, 'b, 'c, 'd, 'e) t -> 'acc
    val map :((('deco, 'b, 'c, 'd, 'e) t, 'f, 'g, 'h, 'i) fixable -> (('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) fixable) -> ('deco, 'f, 'g, 'h, 'i) t -> ('deco, 'b, 'c, 'd, 'e) t
    val foldmap : ('acc -> (('deco, 'c, 'd, 'e, 'f) t, 'g, 'h, 'i, 'j) fixable -> 'acc * (('deco, 'c, 'd, 'e, 'f) t, 'c, 'd, 'e, 'f) fixable) -> (('deco, 'g, 'h, 'i, 'j) t, 'acc, ('deco, 'c, 'd, 'e, 'f) t) fun_foldmap
    val foldmapfghi : ('a, 'acc, 'c) fun_foldmap -> ('d, 'acc, 'e) fun_foldmap -> ('f, 'acc, 'g) fun_foldmap -> ('h, 'acc, 'i) fun_foldmap -> (('deco, 'a, 'd, 'f, 'h) t, 'acc, ('deco, 'c, 'e, 'g, 'i) t) fun_foldmap
    val exists : ((('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) fixable -> bool) -> ('deco, 'b, 'c, 'd, 'e) t -> bool
    val decorate : ('deco_start -> (('deco_end, 'c, 'd, 'e, 'f) t, 'c, 'd, 'e, 'f) fixable -> 'deco_end) -> ('deco_start, 'c, 'd, 'e, 'f) t -> ('deco_end, 'c, 'd, 'e, 'f) t
  end
  module Traverse :
  sig
    val foldmap :
      ((('deco, 'b, 'c, 'd, 'e) t, 'acc, ('deco, 'g, 'h, 'i, 'j) t) fun_foldmap ->
       ((('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) fixable, 'acc, (('deco, 'g, 'h, 'i, 'j) t, 'g, 'h, 'i, 'j) fixable) fun_foldmap ->
       'acc -> (('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) fixable ->
       'acc * (('deco, 'g, 'h, 'i, 'j) t, 'g, 'h, 'i, 'j) fixable) ->
      ('b, 'acc, 'g) fun_foldmap ->
      ('c, 'acc, 'h) fun_foldmap ->
      ('d, 'acc, 'i) fun_foldmap ->
      ('e, 'acc, 'j) fun_foldmap ->
      (('deco, 'b, 'c, 'd, 'e) t, 'acc, ('deco, 'g, 'h, 'i, 'j) t) fun_foldmap
  end
  val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> 'd Pp.printer -> ('deco, 'a, 'b, 'c, 'd) t Pp.printer
end

module type Aligned = sig
  type ('a, 'b, 'c, 'd, 'e) fixable
  type ('deco, 'b, 'c, 'd, 'e) fixed
  val same_shape : ('deco1, 'a, 'b, 'c, 'd) fixed -> ('deco2, 'e, 'f, 'g, 'h) fixed -> bool
  val equals : 'a equals -> 'b equals -> 'c equals -> 'd equals -> ('deco, 'a, 'b, 'c, 'd) fixed equals

  val fold2 : on_mismatch:((('d1, 'a, 'b,'c, 'd) fixed * ( 'd2, 'e,'f, 'g, 'h) fixed), 'acc) fold ->
              (('a * 'e), 'acc) fold ->
              (('b * 'f), 'acc) fold ->
              (('c * 'g), 'acc) fold ->
              (('d * 'h), 'acc) fold ->
              ('d1, 'a, 'b, 'c, 'd) fixed -> ('d2, 'e, 'f, 'g, 'h) fixed -> 'acc -> 'acc

  val for_all2 : on_mismatch:( ('d1, 'a, 'b, 'c, 'd) fixed -> ('d2, 'e, 'f, 'g, 'h) fixed -> bool) ->
                 ('a -> 'e -> bool) ->
                 ('b -> 'f -> bool) ->
                 ('c -> 'g -> bool) ->
                 ('d -> 'h -> bool) ->
                 ('d1, 'a, 'b, 'c, 'd) fixed -> ('d2, 'e, 'f, 'g, 'h) fixed -> bool

  val foldmap2_deco : merge_deco:('deco -> 'deco -> 'deco) ->
                      on_mismatch:(( (('deco, 'a, 'b, 'c, 'd) fixed * ('deco, 'e, 'f, 'g, 'h) fixed), 'acc, ('deco, 'i, 'j, 'k, 'l) fixed) fun_foldmap ) ->
                      (('deco * 'a) * ('deco * 'e), 'acc, 'i) fun_foldmap ->
                      (('deco * 'b) * ('deco * 'f), 'acc, 'j) fun_foldmap ->
                      (('deco * 'c) * ('deco * 'g), 'acc, 'k) fun_foldmap ->
                      (('deco * 'd) * ('deco * 'h), 'acc, 'l) fun_foldmap ->
                      ('deco, 'a, 'b, 'c, 'd) fixed * ('deco, 'e, 'f, 'g, 'h) fixed -> 'acc -> 'acc * ('deco, 'i, 'j, 'k, 'l) fixed

  val foldmap2 : on_mismatch:(( (('deco, 'a, 'b, 'c, 'd) fixed * ('deco, 'e, 'f, 'g, 'h) fixed), 'acc, ('deco, 'i, 'j, 'k, 'l) fixed) fun_foldmap ) ->
                 ('a * 'e, 'acc, 'i) fun_foldmap ->
                 ('b * 'f, 'acc, 'j) fun_foldmap ->
                 ('c * 'g, 'acc, 'k) fun_foldmap ->
                 ('d * 'h, 'acc, 'l) fun_foldmap ->
                 ('deco, 'a, 'b, 'c, 'd) fixed * ('deco, 'e, 'f, 'g, 'h) fixed -> 'acc -> 'acc * ('deco, 'i, 'j, 'k, 'l) fixed
end

module type Comparor = sig
  type ('a, 'b, 'c, 'd, 'e) fixable
  type ('deco, 'b, 'c, 'd, 'e) fixed
  val compare : 'b compare -> 'c compare -> 'd compare -> 'e compare -> ('deco, 'b, 'c, 'd, 'e) fixed compare
end
