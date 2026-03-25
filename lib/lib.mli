open T
(* permet de décrire des parcours récursifs de types explicitement récursifs *)
module Fix3 : functor (F : Fixables3.Fixable3) -> Fixables3.Fixed3 with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) F.t

module Fix4 :
  functor (F : Fixables4.Fixable4) ->
    sig
      type ('deco, 'a, 'b, 'c) t (* = F of 'deco * (('deco, 'a, 'b, 'c) t, 'a, 'b, 'c) F.t *)
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

module FixMut :
  functor (F : Fixables4.Fixable4) (G : Fixables3.Fixable3) ->
    sig
      type ('deco, 'a, 'b) t (* = F of 'deco * (('deco, 'a, 'b) t, ('deco, 'a, 'b) u, 'a, 'b) F.t *)
      and ('deco, 'a, 'b) u (* = G of 'deco *  (('deco, 'a, 'b) u, 'a, 'b) G.t *)
      val decoration_u : ('deco, 'b, 'c) u -> 'deco
      val decoration : ('deco, 'b, 'c) t -> 'deco
      val fix_u : (('deco, 'b, 'c) u, 'b, 'c) G.t -> 'deco -> ('deco, 'b, 'c) u
      val fix : (('deco, 'b, 'c) t, ('deco, 'b, 'c) u, 'b, 'c) F.t -> 'deco -> ('deco, 'b, 'c) t
      val unfix_u : ('deco, 'b, 'c) u -> (('deco, 'b, 'c) u, 'b, 'c) G.t
      val unfix : ('deco, 'b, 'c) t -> (('deco, 'b, 'c) t, ('deco, 'b, 'c) u, 'b, 'c) F.t
      val wrap_acc : ((('deco, 'b, 'c) t, ('deco, 'b, 'c) u, 'b, 'c) F.t -> 'd -> 'e * (('deco, 'f, 'g) t, ('deco, 'f, 'g) u, 'f, 'g) F.t) -> ('deco, 'b, 'c) t -> 'd -> 'e * ('deco, 'f, 'g) t
      module SurfaceF : Fixables4.SigSurface4 with type  ('a, 'b, 'c, 'd) t =  ('a, 'b, 'c, 'd) F.t
      module SurfaceG : Fixables3.SigSurface3 with type  ('a, 'b, 'c) t =  ('a, 'b, 'c) G.t
      module Deep :
        sig
          val mapfg_u : ('a -> 'b) -> ('c -> 'd) -> ('deco, 'a, 'c) u -> ('deco, 'b, 'd) u
          val mapfg : ('a -> 'b) -> ('c -> 'd) -> ('deco, 'a, 'c) t -> ('deco, 'b, 'd) t
          val reduce_u : (('acc, 'b, 'c) G.t -> 'acc) -> ('deco, 'b, 'c) u -> 'acc
          val reduce : (('acc, 'acc_u, 'c, 'd) F.t -> 'acc) -> (('acc_u, 'c, 'd) G.t -> 'acc_u) -> ('deco, 'c, 'd) t -> 'acc
          val reduce_u_d : ('deco -> ('acc, 'c, 'd) G.t -> 'acc) -> ('deco, 'c, 'd) u -> 'acc
          val reduce_d : ('deco -> ('b, 'c, 'd, 'e) F.t -> 'b) -> ('deco -> ('c, 'd, 'e) G.t -> 'c) -> ('deco, 'd, 'e) t -> 'b
          val reduce_de : ('deco -> ('b, ('deco, 'c, 'd) u, 'c, 'd) F.t -> 'b) -> ('deco, 'c, 'd) t -> 'b
          val foldmapfg_u : ('a, 'acc, 'c) fun_foldmap -> ('d, 'acc, 'e) fun_foldmap -> (('f, 'a, 'd) u, 'acc, ('f, 'c, 'e) u) fun_foldmap
          val foldmapfg : ('a, 'acc, 'c) fun_foldmap -> ('d, 'acc, 'e) fun_foldmap -> (('f, 'a, 'd) t, 'acc, ('f, 'c, 'e) t) fun_foldmap
          val map : ((('a, 'b, 'c) t, ('a, 'd, 'e) u, 'd, 'e) F.t -> (('a, 'b, 'c) t, ('a, 'b, 'c) u, 'b, 'c) F.t) -> ('a, 'd, 'e) t -> ('a, 'b, 'c) t
          val decorate_u : ('a -> (('b, 'c, 'd) u, 'c, 'd) G.t -> 'b) -> ('a, 'c, 'd) u -> ('b, 'c, 'd) u
          val decorate : ('a -> (('b, 'c, 'd) t, ('b, 'c, 'd) u, 'c, 'd) F.t -> 'b) -> ('a -> (('b, 'c, 'd) u, 'c, 'd) G.t -> 'b) -> ('a, 'c, 'd) t -> ('b, 'c, 'd) t
          val decorate_foldmap_u : ('deco_start -> (('deco_end, 'c, 'd) u, 'c, 'd) G.t -> 'acc -> 'acc * 'deco_end) -> ('deco_start, 'c, 'd) u -> 'acc -> 'acc * ('deco_end, 'c, 'd) u
          val decorate_foldmap : ('deco_start -> (('deco_end, 'c, 'd) t, ('deco_end, 'c, 'd) u, 'c, 'd) F.t -> 'acc -> 'acc * 'deco_end) ->
                                 ('deco_start -> (('deco_end, 'c, 'd) u, 'c, 'd) G.t -> 'acc -> 'acc * 'deco_end) -> ('deco_start, 'c, 'd) t -> 'acc -> 'acc * ('deco_end, 'c, 'd) t
        end
      module Traverse :
        sig
          val decorate_foldmap_u :
            ( ((('deco_start, 'b, 'c) u, 'acc, ('deco_end, 'f, 'g) u) fun_foldmap) ->
              ((('deco_start, 'b, 'c) u, 'b, 'c) G.t, 'acc, (('deco_end, 'f, 'g) u, 'f, 'g) G.t) fun_foldmap ->
              'acc -> 'deco_start -> (('deco_start, 'b, 'c) u, 'b, 'c) G.t ->
              'acc * 'deco_end * (('deco_end, 'f, 'g) u, 'f, 'g) G.t) ->
            ('deco_start -> ('b, 'acc, 'f) fun_foldmap) ->
            ('deco_start -> ('c, 'acc, 'g) fun_foldmap) ->
            (('deco_start, 'b, 'c) u, 'acc, ('deco_end, 'f, 'g) u) fun_foldmap
          val decorate_foldmap :
            ((('deco_begin, 'b, 'c) t, 'acc, ('deco_end, 'f, 'g) t) fun_foldmap ->
             (('deco_begin, 'h, 'i) u, 'acc, ('j, 'k, 'l) u) fun_foldmap ->
             ((('deco_begin, 'b, 'c) t, ('deco_begin, 'h, 'i) u, 'm, 'n) F.t, 'acc, (('deco_end, 'f, 'g) t, ('j, 'k, 'l) u, 'o, 'p) F.t) fun_foldmap ->
             'acc ->
             'deco_begin ->
             (('deco_begin, 'b, 'c) t, ('deco_begin, 'b, 'c) u, 'b, 'c) F.t -> 'acc * 'deco_end * (('deco_end, 'f, 'g) t, ('deco_end, 'f, 'g) u, 'f, 'g) F.t) ->
            ((('deco_begin, 'h, 'i) u, 'acc, ('j, 'k, 'l) u) fun_foldmap ->
             ((('deco_begin, 'h, 'i) u, 'm, 'n) G.t, 'acc, (('j, 'k, 'l) u, 'o, 'p) G.t) fun_foldmap ->
             'acc -> 'deco_begin -> (('deco_begin, 'h, 'i) u, 'h, 'i) G.t -> 'acc * 'j * (('j, 'k, 'l) u, 'k, 'l) G.t) ->
            ('deco_begin -> ('m, 'acc, 'o) fun_foldmap)->
            ('deco_begin -> ('n, 'acc, 'p) fun_foldmap)->
            (('deco_begin, 'b, 'c) t, 'acc, ('deco_end, 'f, 'g) t) fun_foldmap
          val foldmap_u :
            ((('deco, 'b, 'c) u, 'acc, ('deco, 'e, 'f) u) fun_foldmap ->
             ((('deco, 'b, 'c) u, 'g, 'h) G.t, 'acc, (('deco, 'e, 'f) u, 'i, 'j) G.t) fun_foldmap ->
             'acc -> (('deco, 'b, 'c) u, 'b, 'c) G.t -> 'acc * (('deco, 'e, 'f) u, 'e, 'f) G.t) ->
            ('h, 'acc, 'j) fun_foldmap-> ('g, 'acc, 'i) fun_foldmap-> (('deco, 'b, 'c) u, 'acc, ('deco, 'e, 'f) u) fun_foldmap
          val foldmap :
            ( (('deco, 'b, 'c) t, 'acc, ('deco, 'e, 'f) t) fun_foldmap ->
              (('deco, 'g, 'h) u, 'acc, ('deco, 'i, 'j) u) fun_foldmap ->
              ((('deco, 'b, 'c) t, ('deco, 'g, 'h) u, 'k, 'l) F.t, 'acc, (('deco, 'e, 'f) t, ('deco, 'i, 'j) u, 'm, 'n) F.t) fun_foldmap ->
              'acc -> (('deco, 'b, 'c) t, ('deco, 'b, 'c) u, 'b, 'c) F.t -> 'acc * (('deco, 'e, 'f) t, ('deco, 'e, 'f) u, 'e, 'f) F.t) ->
            ((('deco, 'g, 'h) u, 'acc, ('deco, 'i, 'j) u) fun_foldmap ->
             ((('deco, 'g, 'h) u, 'k, 'l) G.t, 'acc, (('deco, 'i, 'j) u, 'm, 'n) G.t) fun_foldmap -> 'acc -> (('deco, 'g, 'h) u, 'g, 'h) G.t -> 'acc * (('deco, 'i, 'j) u, 'i, 'j) G.t) ->
            ('k, 'acc, 'm) fun_foldmap -> ('l, 'acc, 'n) fun_foldmap -> (('deco, 'b, 'c) t, 'acc, ('deco, 'e, 'f) t) fun_foldmap
        end
      val ppu : 'a Pp.printer -> 'b Pp.printer -> ('deco, 'a, 'b) u Pp.printer
      val pp :  'a Pp.printer -> 'b Pp.printer -> ('deco, 'a, 'b) t Pp.printer
    end
