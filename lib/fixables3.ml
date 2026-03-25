open T

module type Fixable3 = sig
  type ('a, 'b, 'c) t
  val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> ('a, 'b, 'c) t Pp.printer
  module Make : functor (F:Applicatives.Applicative) -> sig
    val foldmap : ('a -> 'ra F.t) -> ('b -> 'rb F.t) -> ('c -> 'rc F.t) -> ('a, 'b, 'c) t -> ('ra, 'rb, 'rc) t F.t
  end
end

module type Alignable3 = sig
  type ('a, 'b, 'c) t
  val align : ('a1, 'b1, 'c1) t -> ('a2, 'b2, 'c2) t -> ( ('a1 * 'a2), ('b1 * 'b2), ('c1 * 'c2) ) t option
end

module type Comparable3 = sig
  include Alignable3
  val tag : ('a, 'b, 'c) t -> int
end

module type SigSurface3 = sig
  type ('a, 'b, 'c) t
  val fold : ('a, 'acc) fold -> ('b, 'acc) fold -> ('c, 'acc) fold -> (('a, 'b, 'c) t, 'acc) fold
  val map : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('c1 -> 'c2) -> ('a1, 'b1, 'c1) t -> ('a2, 'b2, 'c2) t
  val foldmap : ('a, 'acc, 'a2) fun_foldmap -> ('b, 'acc, 'b2) fun_foldmap -> ('c, 'acc, 'c2) fun_foldmap -> (('a, 'b, 'c) t, 'acc, ('a2, 'b2, 'c2) t) fun_foldmap
end

module type Fixed3 = sig
  type ('a, 'b, 'c) fixable
  type ('deco, 'a, 'b) t (* = F of 'deco * (('deco, 'a, 'b) t, 'a, 'b) F.t *)
      val decoration : ('deco, 'b, 'c) t -> 'deco
      val fix : (('deco, 'b, 'c) t, 'b, 'c) fixable -> 'deco -> ('deco, 'b, 'c) t
      val unfix : ('deco, 'b, 'c) t -> (('deco, 'b, 'c) t, 'b, 'c) fixable
      val wrap_acc : ((('deco, 'b, 'c) t, 'b, 'c) fixable -> 'e -> 'f * (('deco, 'g, 'h) t, 'g, 'h) fixable) -> ('deco, 'b, 'c) t -> 'e -> 'f * ('deco, 'g, 'h) t
     (* réécritures par une Applicative *)
      module MakeApp :
        functor (A : Applicatives.Applicative) ->
          sig
            val foldmap : ('a -> 'b A.t) -> ('c -> 'd A.t) -> ('e, 'a, 'c) t -> ('e, 'b, 'd) t A.t
          end
     (* réécritures en surface du type *)
      module Surface : SigSurface3 with type  ('a, 'b, 'c) t =  ('a, 'b, 'c) fixable
     (* réécritures en profondeur du type *)
      module Deep :
        sig
          val mapfg : ('a -> 'b) -> ('c -> 'd) -> ('deco, 'a, 'c) t -> ('deco, 'b, 'd) t
          val foldfg : ('a, 'acc) fold -> ('c, 'acc) fold -> (('deco, 'a, 'c) t, 'acc) fold
          val reduce : (('a, 'b, 'c) fixable -> 'a) -> ('deco, 'b, 'c) t -> 'a
          val map : ((('deco, 'b, 'c) t, 'd, 'e) fixable -> (('deco, 'b, 'c) t, 'b, 'c) fixable) -> ('deco, 'd, 'e) t -> ('deco, 'b, 'c) t
          val foldmapfg : ('a, 'acc, 'c) fun_foldmap -> ('d, 'acc, 'e) fun_foldmap -> (('deco, 'a, 'd) t, 'acc, ('deco, 'c, 'e) t) fun_foldmap
          val foldmap : ('acc -> (('b, 'c, 'd) t, 'e, 'f) fixable -> 'acc * (('b, 'c, 'd) t, 'c, 'd) fixable) -> ('b, 'e, 'f) t -> 'acc -> 'acc * ('b, 'c, 'd) t
          val exists : ( (('deco, 'a, 'b) t, 'a, 'b) fixable -> bool) -> ('deco, 'a, 'b) t -> bool
          val decorate : ('deco_start -> (('deco_end, 'c, 'd) t, 'c, 'd) fixable -> 'deco_end) -> ('deco_start, 'c, 'd) t -> ('deco_end, 'c, 'd) t
        end
      (* module Traverse qui permet de contrôler le parcours de l'AST. La première fonction foldmap sur ('a, 'b, 'c) t, 'b, 'c) F.t (une valeur défixée) et kes fonctions qu'elle reçoit en paramètres permettent de contrôler le parcours : la première fonction permet de rentrer dans ses enfants, la seconde gère le cas par défaut le parcours du cas par défaut (propagation de l'accumulateur en déconstruisant le F.t) puis elle reçoit une valeur et un accumulateur et renvoie accumulateur * valeur *)
      module Traverse :
        sig
          val foldmap :
            (  (('deco, 'b, 'c) t, 'acc, ('deco, 'e, 'f) t) fun_foldmap ->
               ((('deco, 'b, 'c) t, 'b, 'c) fixable, 'acc, (('deco, 'e, 'f) t, 'e, 'f) fixable) fun_foldmap -> 'acc -> (('deco, 'b, 'c) t, 'b, 'c) fixable ->
               'acc * (('deco, 'e, 'f) t, 'e, 'f) fixable) ->
            ('b, 'acc, 'e) fun_foldmap -> ('c, 'acc, 'f) fun_foldmap ->
            (('deco, 'b, 'c) t, 'acc, ('deco, 'e, 'f) t) fun_foldmap
        end
      val pp : 'a Pp.printer -> 'b Pp.printer -> ('c, 'a, 'b) t Pp.printer
end

module type Aligned3 = sig
  type ('a, 'b, 'c) fixable
  type ('deco, 'b, 'c) fixed
  val same_shape : ('deco1, 'b, 'c) fixed -> ('deco2, 'e, 'f) fixed -> bool
  val equals : 'b equals -> 'c equals -> ('deco, 'b, 'c) fixed equals
  val fold2 : on_mismatch:((('d1,'b,'c) fixed * ( 'd2,'e,'f) fixed), 'acc) fold -> (('b * 'e), 'acc) fold -> (('c * 'f), 'acc) fold -> ('d1,'b,'c) fixed -> ('d2,'e,'f) fixed -> 'acc -> 'acc
  val for_all2 : on_mismatch:( ('d1, 'b, 'c) fixed -> ('d2, 'e, 'f) fixed -> bool) -> ('b -> 'e -> bool) -> ('c -> 'f -> bool) -> ('d1,'b,'c) fixed -> ('d2,'e,'f) fixed -> bool
  val foldmap2_deco : merge_deco:('deco -> 'deco -> 'deco) ->
                      on_mismatch:(( (('deco, 'b, 'c) fixed * ('deco, 'e, 'f) fixed), 'acc, ('deco, 'h, 'i) fixed) fun_foldmap ) ->
                      (('deco * 'b) * ('deco * 'e), 'acc, 'h) fun_foldmap ->
                      (('deco * 'c) * ('deco * 'f), 'acc, 'i) fun_foldmap ->
                      ('deco, 'b, 'c) fixed * ('deco, 'e, 'f) fixed -> 'acc -> 'acc * ('deco, 'h, 'i) fixed
  val foldmap2 : on_mismatch:(( (('deco, 'b, 'c) fixed * ('deco, 'e, 'f) fixed), 'acc, ('deco, 'h, 'i) fixed) fun_foldmap ) ->
                 ('b * 'e, 'acc, 'h) fun_foldmap -> ('c * 'f, 'acc, 'i) fun_foldmap ->
                 ('deco, 'b, 'c) fixed * ('deco, 'e, 'f) fixed -> 'acc -> 'acc * ('deco, 'h, 'i) fixed
end
module type Comparor3 = sig
  type ('a, 'b, 'c) fixable
  type ('deco, 'b, 'c) fixed
  val compare : 'b compare -> 'c compare -> ('d, 'b, 'c) fixed compare
end
