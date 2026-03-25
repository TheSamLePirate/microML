open T

module type Fixable = sig
  type ('a, 'b, 'c, 'd) t
  val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> 'd Pp.printer -> ('a, 'b, 'c, 'd) t Pp.printer
  module Make : functor (F:Applicatives.Applicative) -> sig
    val foldmap : ('a -> 'ra F.t) -> ('b -> 'rb F.t) -> ('c -> 'rc F.t) -> ('d -> 'rd F.t) -> ('a, 'b, 'c, 'd) t -> ('ra, 'rb, 'rc, 'rd) t F.t
  end
end

module type Alignable = sig
  type ('a, 'b, 'c, 'd) t
  val align : ('a1, 'b1, 'c1, 'd1) t -> ('a2, 'b2, 'c2, 'd2) t -> ( ('a1 * 'a2), ('b1 * 'b2), ('c1 * 'c2), ('d1 * 'd2)) t option
end

module type Comparable = sig
  include Alignable
  val tag : ('a, 'b, 'c, 'd) t -> int
end

module type SigSurface = sig
  type ('a, 'b, 'c, 'd) t
  val fold : ('a, 'acc) fold -> ('b, 'acc) fold -> ('c, 'acc) fold -> ('d, 'acc) fold -> (('a, 'b, 'c, 'd) t, 'acc) fold
  val map : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('c1 -> 'c2) -> ('d1 -> 'd2) ->  ('a1, 'b1, 'c1, 'd1) t -> ('a2, 'b2, 'c2, 'd2) t
  val foldmap : ('a, 'acc, 'a2) fun_foldmap-> ('b, 'acc, 'b2) fun_foldmap -> ('c, 'acc, 'c2) fun_foldmap -> ('d, 'acc, 'd2) fun_foldmap -> (('a, 'b, 'c, 'd) t, 'acc, ('a2, 'b2, 'c2, 'd2) t) fun_foldmap
end

module type Fixed = sig
  type ('a, 'b, 'c, 'd) fixable
  module Fixed5 : Fixables5.Fixed with type ('a, 'b, 'c, 'd, 'e) fixable = ('a, 'b, 'c, 'd) fixable
  type ('deco, 'a, 'b, 'c) t = ('deco, 'a, 'b, 'c, unit)  Fixed5.t

  val decoration : ('deco, 'b, 'c, 'd) t -> 'deco
  val fix : (('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) fixable -> 'deco -> ('deco, 'b, 'c, 'd) t
  val unfix : ('deco, 'b, 'c, 'd) t -> (('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) fixable
  val wrap_acc : ((('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) fixable -> 'e -> 'f * (('deco, 'g, 'h, 'i) t, 'g, 'h, 'i) fixable) -> ('deco, 'b, 'c, 'd) t -> 'e -> 'f * ('deco, 'g, 'h, 'i) t
  module MakeApp :
  functor (A : Applicatives.Applicative) -> sig
    val foldmap : ('a -> 'b A.t) -> ('c -> 'd A.t) -> ('e -> 'f A.t) -> ('deco, 'a, 'c, 'e) t -> ('deco, 'b, 'd, 'f) t A.t
  end
  module Surface : SigSurface with type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) fixable
  module Deep :
  sig
    val mapfg : ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('deco, 'a, 'c, 'e) t -> ('deco, 'b, 'd, 'f) t
    val foldfg : ('a, 'acc) fold -> ('c, 'acc) fold -> ('d, 'acc) fold -> (('deco, 'a, 'c, 'd) t, 'acc) fold
    val reduce : (('acc, 'b, 'c, 'd) fixable -> 'acc) -> ('deco, 'b, 'c, 'd) t -> 'acc
    val map : ((('a, 'b, 'c, 'd) t, 'e, 'f, 'g) fixable -> (('a, 'b, 'c, 'd) t, 'b, 'c, 'd) fixable) -> ('a, 'e, 'f, 'g) t -> ('a, 'b, 'c, 'd) t
    val foldmap : ('acc -> (('deco, 'c, 'd, 'e) t, 'f, 'g, 'h) fixable -> 'acc * (('deco, 'c, 'd, 'e) t, 'c, 'd, 'e) fixable) -> (('deco, 'f, 'g, 'h) t, 'acc, ('deco, 'c, 'd, 'e) t) fun_foldmap
    val foldmapfgh : ('a -> 'acc -> 'acc * 'c) -> ('d -> 'acc -> 'acc * 'e) -> ('f -> 'acc -> 'acc * 'g) -> (('h, 'a, 'd, 'f) t, 'acc, ('h, 'c, 'e, 'g) t) fun_foldmap
    val exists : ((('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) fixable -> bool) -> ('deco, 'b, 'c, 'd) t -> bool
  end
  module Traverse : sig
    val foldmap :
      (
        (('deco, 'b, 'c, 'd) t, 'acc, ('deco, 'f, 'g, 'h) t) fun_foldmap->
        ((('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) fixable, 'acc, (('deco, 'f, 'g, 'h) t, 'f, 'g, 'h) fixable) fun_foldmap ->
        'acc -> (('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) fixable ->
        'acc * (('deco, 'f, 'g, 'h) t, 'f, 'g, 'h) fixable) ->
      ('b, 'acc, 'f) fun_foldmap ->
      ('c, 'acc, 'g) fun_foldmap ->
      ('d, 'acc, 'h) fun_foldmap ->
      (('deco, 'b, 'c, 'd) t, 'acc, ('deco, 'f, 'g, 'h) t ) fun_foldmap
  end
  val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> ('deco, 'a, 'b, 'c) t Pp.printer
end

module type Aligned = sig
  type ('a, 'b, 'c, 'd) fixable
  type ('deco, 'b, 'c, 'd) fixed
  val same_shape : ('deco1, 'b, 'c, 'd) fixed -> ('deco2, 'e, 'f, 'g) fixed -> bool
  val equals : 'b equals -> 'c equals -> 'd equals -> ('deco, 'b, 'c, 'd) fixed equals
  val fold2 : on_mismatch:((('d1,'b,'c, 'd) fixed * ( 'd2,'e,'f, 'g) fixed), 'acc) fold -> (('b * 'e), 'acc) fold -> (('c * 'f), 'acc) fold -> (('d * 'g), 'acc) fold -> ('d1,'b,'c, 'd) fixed -> ('d2,'e,'f, 'g) fixed -> 'acc -> 'acc
  val for_all2 : on_mismatch:( ('d1, 'b, 'c, 'd) fixed -> ('d2, 'e, 'f, 'g) fixed -> bool) -> ('b -> 'e -> bool) -> ('c -> 'f -> bool) -> ('d -> 'g -> bool) -> ('d1, 'b, 'c, 'd) fixed -> ('d2, 'e, 'f, 'g) fixed -> bool
  val foldmap2_deco : merge_deco:('deco -> 'deco -> 'deco) ->
                      on_mismatch:(( (('deco, 'b, 'c, 'd) fixed * ('deco, 'e, 'f, 'g) fixed), 'acc, ('deco, 'h, 'i, 'j) fixed) fun_foldmap ) ->
                      (('deco * 'b) * ('deco * 'e), 'acc, 'h) fun_foldmap ->
                      (('deco * 'c) * ('deco * 'f), 'acc, 'i) fun_foldmap ->
                      (('deco * 'd) * ('deco * 'g), 'acc, 'j) fun_foldmap ->
                      ('deco, 'b, 'c, 'd) fixed * ('deco, 'e, 'f, 'g) fixed -> 'acc -> 'acc * ('deco, 'h, 'i, 'j) fixed
  val foldmap2 : on_mismatch:(( (('deco, 'b, 'c, 'd) fixed * ('deco, 'e, 'f, 'g) fixed), 'acc, ('deco, 'h, 'i, 'j) fixed) fun_foldmap ) ->
                 ('b * 'e, 'acc, 'h) fun_foldmap ->
                 ('c * 'f, 'acc, 'i) fun_foldmap ->
                 ('d * 'g, 'acc, 'j) fun_foldmap ->
                 ('deco, 'b, 'c, 'd) fixed * ('deco, 'e, 'f, 'g) fixed -> 'acc -> 'acc * ('deco, 'h, 'i, 'j) fixed
end

module type Comparor = sig
  type ('a, 'b, 'c, 'd) fixable
  type ('deco, 'b, 'c, 'd) fixed
  val compare : 'b compare -> 'c compare -> 'd compare -> ('deco, 'b, 'c, 'd) fixed compare
end
