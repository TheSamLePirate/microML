open T

module MakeSurface : functor (F : Fixables4.Fixable4) -> Fixables4.SigSurface4 with type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) F.t

module Fix :
  functor (F : Fixables4.Fixable4) ->
    sig
      type ('deco, 'a, 'b, 'c) t
      type ('a, 'b, 'c, 'd) fixable = ('a, 'b, 'c, 'd) F.t
      val decoration : ('deco, 'b, 'c, 'd) t -> 'deco
      val fix : (('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t -> 'deco -> ('deco, 'b, 'c, 'd) t
      val unfix : ('deco, 'b, 'c, 'd) t -> (('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t
      val wrap_acc : ((('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t -> 'e -> 'f * (('deco, 'g, 'h, 'i) t, 'g, 'h, 'i) F.t) -> ('deco, 'b, 'c, 'd) t -> 'e -> 'f * ('deco, 'g, 'h, 'i) t
      module MakeApp : functor (A : Applicatives.Applicative) -> sig
            val foldmap : ('a -> 'b A.t) -> ('c -> 'd A.t) -> ('e -> 'f A.t) -> ('deco, 'a, 'c, 'e) t -> ('deco, 'b, 'd, 'f) t A.t
          end
      module Surface : Fixables4.SigSurface4 with type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) F.t
      module Deep :
        sig
          val mapfg : ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('deco, 'a, 'c, 'e) t -> ('deco, 'b, 'd, 'f) t
          val foldfg : ('a, 'acc) fold -> ('c, 'acc) fold -> ('d, 'acc) fold -> (('deco, 'a, 'c, 'd) t, 'acc) fold
          val reduce : (('acc, 'b, 'c, 'd) F.t -> 'acc) -> ('deco, 'b, 'c, 'd) t -> 'acc
          val map : ((('a, 'b, 'c, 'd) t, 'e, 'f, 'g) F.t -> (('a, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t) -> ('a, 'e, 'f, 'g) t -> ('a, 'b, 'c, 'd) t
          val foldmap : ('acc -> (('deco, 'c, 'd, 'e) t, 'f, 'g, 'h) F.t -> 'acc * (('deco, 'c, 'd, 'e) t, 'c, 'd, 'e) F.t) -> (('deco, 'f, 'g, 'h) t, 'acc, ('deco, 'c, 'd, 'e) t) fun_foldmap
          val foldmapfgh : ('a -> 'acc -> 'acc * 'c) -> ('d -> 'acc -> 'acc * 'e) -> ('f -> 'acc -> 'acc * 'g) -> (('h, 'a, 'd, 'f) t, 'acc, ('h, 'c, 'e, 'g) t) fun_foldmap
          val exists : ((('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t -> bool) -> ('deco, 'b, 'c, 'd) t -> bool
        end
      module Traverse :
        sig
          val foldmap :
            (
              (('deco, 'b, 'c, 'd) t, 'acc, ('deco, 'f, 'g, 'h) t) fun_foldmap->
              ((('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t, 'acc, (('deco, 'f, 'g, 'h) t, 'f, 'g, 'h) F.t) fun_foldmap ->
             'acc -> (('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t ->
             'acc * (('deco, 'f, 'g, 'h) t, 'f, 'g, 'h) F.t) ->
            ('b, 'acc, 'f) fun_foldmap ->
            ('c, 'acc, 'g) fun_foldmap ->
            ('d, 'acc, 'h) fun_foldmap ->
            (('deco, 'b, 'c, 'd) t, 'acc, ('deco, 'f, 'g, 'h) t ) fun_foldmap
        end
      val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> ('deco, 'a, 'b, 'c) t Pp.printer
    end
