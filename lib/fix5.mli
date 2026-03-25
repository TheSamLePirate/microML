open T

module MakeSurface : functor (F : Fixables5.Fixable5) -> Fixables5.SigSurface5 with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) F.t

module Fix :
  functor (F : Fixables5.Fixable5) ->
    sig
      type ('deco, 'a, 'b, 'c, 'd) t (* = F of 'deco * (('deco, 'a, 'b, 'c, 'd) t, 'a, 'b, 'c, 'd) F.t
*)
      val decoration : ('deco, 'b, 'c, 'd, 'e) t -> 'deco
      val fix : (('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t -> 'deco -> ('deco, 'b, 'c, 'd, 'e) t
      val unfix : ('deco, 'b, 'c, 'd, 'e) t -> (('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t
      val wrap_acc : ((('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t -> 'f -> 'g * (('deco, 'h, 'i, 'j, 'k) t, 'h, 'i, 'j, 'k) F.t) -> ('deco, 'b, 'c, 'd, 'e) t -> 'f -> 'g * ('deco, 'h, 'i, 'j, 'k) t
      module MakeApp : functor (A : Applicatives.Applicative) -> sig
            val foldmap : ('a -> 'b A.t) -> ('c -> 'd A.t) -> ('e -> 'f A.t) -> ('g -> 'h A.t) -> ('i, 'a, 'c, 'e, 'g) t -> ('i, 'b, 'd, 'f, 'h) t A.t
          end
      module Surface : Fixables5.SigSurface5 with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) F.t
      module Deep :
        sig
          val mapfg : ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'h) -> ('deco, 'a, 'c, 'e, 'g) t -> ('deco, 'b, 'd, 'f, 'h) t
          val foldfg : ('a, 'acc) fold -> ('c, 'acc) fold -> ('d, 'acc) fold -> ('e, 'acc) fold -> (('deco, 'a, 'c, 'd, 'e) t, 'acc) fold
          val reduce : (('acc, 'b, 'c, 'd, 'e) F.t -> 'acc) -> ('deco, 'b, 'c, 'd, 'e) t -> 'acc
          val map :((('deco, 'b, 'c, 'd, 'e) t, 'f, 'g, 'h, 'i) F.t -> (('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t) -> ('deco, 'f, 'g, 'h, 'i) t -> ('deco, 'b, 'c, 'd, 'e) t
          val foldmap : ('acc -> (('deco, 'c, 'd, 'e, 'f) t, 'g, 'h, 'i, 'j) F.t -> 'acc * (('deco, 'c, 'd, 'e, 'f) t, 'c, 'd, 'e, 'f) F.t) -> (('deco, 'g, 'h, 'i, 'j) t, 'acc, ('deco, 'c, 'd, 'e, 'f) t) fun_foldmap
          val foldmapfghi : ('a, 'acc, 'c) fun_foldmap -> ('d, 'acc, 'e) fun_foldmap -> ('f, 'acc, 'g) fun_foldmap -> ('h, 'acc, 'i) fun_foldmap -> (('deco, 'a, 'd, 'f, 'h) t, 'acc, ('deco, 'c, 'e, 'g, 'i) t) fun_foldmap
          val exists : ((('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t -> bool) -> ('deco, 'b, 'c, 'd, 'e) t -> bool
          val decorate : ('deco_start -> (('deco_end, 'c, 'd, 'e, 'f) t, 'c, 'd, 'e, 'f) F.t -> 'deco_end) -> ('deco_start, 'c, 'd, 'e, 'f) t -> ('deco_end, 'c, 'd, 'e, 'f) t

        end
      module Traverse :
        sig
          val foldmap :
            ((('deco, 'b, 'c, 'd, 'e) t, 'acc, ('deco, 'g, 'h, 'i, 'j) t) fun_foldmap ->
             ((('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t, 'acc, (('deco, 'g, 'h, 'i, 'j) t, 'g, 'h, 'i, 'j) F.t) fun_foldmap ->
             'acc -> (('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t ->
             'acc * (('deco, 'g, 'h, 'i, 'j) t, 'g, 'h, 'i, 'j) F.t) ->
            ('b, 'acc, 'g) fun_foldmap ->
            ('c, 'acc, 'h) fun_foldmap ->
            ('d, 'acc, 'i) fun_foldmap ->
            ('e, 'acc, 'j) fun_foldmap ->
            (('deco, 'b, 'c, 'd, 'e) t, 'acc, ('deco, 'g, 'h, 'i, 'j) t) fun_foldmap
        end
      val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> 'd Pp.printer -> ('deco, 'a, 'b, 'c, 'd) t Pp.printer
    end
