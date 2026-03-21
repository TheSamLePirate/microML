(* permet de décrire des parcours récursifs de types explicitement récursifs *)
module Fix3 :
  functor (F : Fixables.Fixable3) ->
    sig
      type ('deco, 'a, 'b) t (* = F of 'deco * (('deco, 'a, 'b) t, 'a, 'b) F.t *)
      val decoration : ('deco, 'b, 'c) t -> 'deco
      val fix : (('deco, 'b, 'c) t, 'b, 'c) F.t -> 'deco -> ('deco, 'b, 'c) t
      val unfix : ('deco, 'b, 'c) t -> (('deco, 'b, 'c) t, 'b, 'c) F.t
      val wrap_acc : ((('deco, 'b, 'c) t, 'b, 'c) F.t -> 'e -> 'f * (('deco, 'g, 'h) t, 'g, 'h) F.t) -> ('deco, 'b, 'c) t -> 'e -> 'f * ('deco, 'g, 'h) t
     (* réécritures par une Applicative *)
      module MakeApp :
        functor (A : Applicatives.Applicative) ->
          sig
            val foldmap : ('a -> 'b A.t) -> ('c -> 'd A.t) -> ('e, 'a, 'c) t -> ('e, 'b, 'd) t A.t
          end
     (* réécritures en surface du type *)
      module Surface : Fixables.SigSurface3 with type  ('a, 'b, 'c) t =  ('a, 'b, 'c) F.t
     (* réécritures en profondeur du type *)
      module Deep :
        sig
          val mapfg : ('a -> 'b) -> ('c -> 'd) -> ('deco, 'a, 'c) t -> ('deco, 'b, 'd) t
          val foldfg : ('a -> 'acc -> 'acc) -> ('c -> 'acc -> 'acc) -> ('deco, 'a, 'c) t -> 'acc -> 'acc
          val reduce : (('a, 'b, 'c) F.t -> 'a) -> ('deco, 'b, 'c) t -> 'a
          val map : ((('deco, 'b, 'c) t, 'd, 'e) F.t -> (('deco, 'b, 'c) t, 'b, 'c) F.t) -> ('deco, 'd, 'e) t -> ('deco, 'b, 'c) t
          val foldmapfg : ('a -> 'acc -> 'acc * 'c) -> ('d -> 'acc -> 'acc * 'e) -> ('deco, 'a, 'd) t -> 'acc -> 'acc * ('deco, 'c, 'e) t
          val foldmap : ('acc -> (('b, 'c, 'd) t, 'e, 'f) F.t -> 'acc * (('b, 'c, 'd) t, 'c, 'd) F.t) -> ('b, 'e, 'f) t -> 'acc -> 'acc * ('b, 'c, 'd) t
          val decorate : ('deco_start -> (('deco_end, 'c, 'd) t, 'c, 'd) F.t -> 'deco_end) -> 'e -> ('deco_start, 'c, 'd) t -> ('deco_end, 'c, 'd) t
        end
      (* module Traverse qui permet de contrôler le parcours de l'AST. La première fonction foldmap sur ('a, 'b, 'c) t, 'b, 'c) F.t (une valeur défixée) et kes fonctions qu'elle reçoit en paramètres permettent de contrôler le parcours : la première fonction permet de rentrer dans ses enfants, la seconde gère le cas par défaut le parcours du cas par défaut (propagation de l'accumulateur en déconstruisant le F.t) puis elle reçoit une valeur et un accumulateur et renvoie accumulateur * valeur *)
      module Traverse :
        sig
          val foldmap :
            (  (('deco, 'b, 'c) t -> 'acc -> 'acc * ('deco, 'e, 'f) t) ->
               ((('deco, 'b, 'c) t, 'b, 'c) F.t -> 'acc -> 'acc * (('deco, 'e, 'f) t, 'e, 'f) F.t) -> 'acc ->
               (('deco, 'b, 'c) t, 'b, 'c) F.t ->
               'acc * (('deco, 'e, 'f) t, 'e, 'f) F.t) ->
            ('b -> 'acc -> 'acc * 'e) ->
            ('c -> 'acc -> 'acc * 'f) ->
            ('deco, 'b, 'c) t -> 'acc -> 'acc * ('deco, 'e, 'f) t
        end
      val pp : 'a Pp.printer -> 'b Pp.printer -> ('c, 'a, 'b) t Pp.printer
    end
module Fix4 :
  functor (F : Fixables.Fixable4) ->
    sig
      type ('deco, 'a, 'b, 'c) t (* = F of 'deco * (('deco, 'a, 'b, 'c) t, 'a, 'b, 'c) F.t *)
      val decoration : ('deco, 'b, 'c, 'd) t -> 'deco
      val fix : (('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t -> 'deco -> ('deco, 'b, 'c, 'd) t
      val unfix : ('deco, 'b, 'c, 'd) t -> (('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t
      val wrap_acc : ((('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t -> 'e -> 'f * (('deco, 'g, 'h, 'i) t, 'g, 'h, 'i) F.t) -> ('deco, 'b, 'c, 'd) t -> 'e -> 'f * ('deco, 'g, 'h, 'i) t
      module MakeApp : functor (A : Applicatives.Applicative) -> sig
            val foldmap : ('a -> 'b A.t) -> ('c -> 'd A.t) -> ('e -> 'f A.t) -> ('deco, 'a, 'c, 'e) t -> ('deco, 'b, 'd, 'f) t A.t
          end
      module Surface : Fixables.SigSurface4 with type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) F.t
      module Deep :
        sig
          val mapfg : ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('deco, 'a, 'c, 'e) t -> ('deco, 'b, 'd, 'f) t
          val foldfg : ('a -> 'acc -> 'acc) -> ('c -> 'acc -> 'acc) -> ('d -> 'acc -> 'acc) -> ('deco, 'a, 'c, 'd) t -> 'acc -> 'acc
          val reduce : (('acc, 'b, 'c, 'd) F.t -> 'acc) -> ('deco, 'b, 'c, 'd) t -> 'acc
          val map : ((('a, 'b, 'c, 'd) t, 'e, 'f, 'g) F.t -> (('a, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t) -> ('a, 'e, 'f, 'g) t -> ('a, 'b, 'c, 'd) t
          val foldmap : ('acc -> (('deco, 'c, 'd, 'e) t, 'f, 'g, 'h) F.t -> 'acc * (('deco, 'c, 'd, 'e) t, 'c, 'd, 'e) F.t) -> ('deco, 'f, 'g, 'h) t -> 'acc -> 'acc * ('deco, 'c, 'd, 'e) t
          val foldmapfgh : ('a -> 'acc -> 'acc * 'c) -> ('d -> 'acc -> 'acc * 'e) -> ('f -> 'acc -> 'acc * 'g) -> ('h, 'a, 'd, 'f) t -> 'acc -> 'acc * ('h, 'c, 'e, 'g) t
          val exists : ((('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t -> bool) -> ('deco, 'b, 'c, 'd) t -> bool
        end
      module Traverse :
        sig
          val foldmap :
            (
              (('deco, 'b, 'c, 'd) t -> 'acc -> 'acc * ('deco, 'f, 'g, 'h) t) ->
              ((('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t ->
              'acc -> 'acc * (('deco, 'f, 'g, 'h) t, 'f, 'g, 'h) F.t) ->
             'acc ->
             (('deco, 'b, 'c, 'd) t, 'b, 'c, 'd) F.t ->
             'acc * (('deco, 'f, 'g, 'h) t, 'f, 'g, 'h) F.t) ->
            ('b -> 'acc -> 'acc * 'f) ->
            ('c -> 'acc -> 'acc * 'g) ->
            ('d -> 'acc -> 'acc * 'h) ->
            ('deco, 'b, 'c, 'd) t -> 'acc -> 'acc * ('deco, 'f, 'g, 'h) t
        end
      val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> ('deco, 'a, 'b, 'c) t Pp.printer
    end
module Fix5 :
  functor (F : Fixables.Fixable5) ->
    sig
      type ('deco, 'a, 'b, 'c, 'd) t (* = F of 'deco * (('deco, 'a, 'b, 'c, 'd) t, 'a, 'b, 'c, 'd) F.t
*)
      val decoration : ('a, 'b, 'c, 'd, 'e) t -> 'a
      val fix : (('a, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t -> 'a -> ('a, 'b, 'c, 'd, 'e) t
      val unfix : ('a, 'b, 'c, 'd, 'e) t -> (('a, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t
      val wrap_acc : ((('a, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t -> 'f -> 'g * (('a, 'h, 'i, 'j, 'k) t, 'h, 'i, 'j, 'k) F.t) -> ('a, 'b, 'c, 'd, 'e) t -> 'f -> 'g * ('a, 'h, 'i, 'j, 'k) t
      module MakeApp : functor (A : Applicatives.Applicative) -> sig
            val foldmap : ('a -> 'b A.t) -> ('c -> 'd A.t) -> ('e -> 'f A.t) -> ('g -> 'h A.t) -> ('i, 'a, 'c, 'e, 'g) t -> ('i, 'b, 'd, 'f, 'h) t A.t
          end
      module Surface : Fixables.SigSurface5 with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) F.t
      module Deep :
        sig
          val mapfg : ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> ('g -> 'h) -> ('i, 'a, 'c, 'e, 'g) t -> ('i, 'b, 'd, 'f, 'h) t
          val foldfg : ('a -> 'acc -> 'acc) -> ('c -> 'acc -> 'acc) -> ('d -> 'acc -> 'acc) -> ('e -> 'acc -> 'acc) -> ('deco, 'a, 'c, 'd, 'e) t -> 'acc -> 'acc
          val reduce : (('acc, 'b, 'c, 'd, 'e) F.t -> 'acc) -> ('deco, 'b, 'c, 'd, 'e) t -> 'acc
          val map :((('deco, 'b, 'c, 'd, 'e) t, 'f, 'g, 'h, 'i) F.t -> (('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t) -> ('deco, 'f, 'g, 'h, 'i) t -> ('deco, 'b, 'c, 'd, 'e) t
          val foldmap : ('acc -> (('deco, 'c, 'd, 'e, 'f) t, 'g, 'h, 'i, 'j) F.t -> 'acc * (('deco, 'c, 'd, 'e, 'f) t, 'c, 'd, 'e, 'f) F.t) -> ('deco, 'g, 'h, 'i, 'j) t -> 'acc -> 'acc * ('deco, 'c, 'd, 'e, 'f) t
          val foldmapfghi : ('a -> 'acc -> 'acc * 'c) -> ('d -> 'acc -> 'acc * 'e) -> ('f -> 'acc -> 'acc * 'g) -> ('h -> 'acc -> 'acc * 'i) -> ('deco, 'a, 'd, 'f, 'h) t -> 'acc -> 'acc * ('deco, 'c, 'e, 'g, 'i) t
          val exists : ((('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t -> bool) -> ('deco, 'b, 'c, 'd, 'e) t -> bool
        end
      module Traverse :
        sig
          val foldmap :
            ((('deco, 'b, 'c, 'd, 'e) t -> 'acc -> 'acc * ('deco, 'g, 'h, 'i, 'j) t) ->
             ((('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t -> 'acc -> 'acc * (('deco, 'g, 'h, 'i, 'j) t, 'g, 'h, 'i, 'j) F.t) ->
             'acc -> (('deco, 'b, 'c, 'd, 'e) t, 'b, 'c, 'd, 'e) F.t ->
             'acc * (('deco, 'g, 'h, 'i, 'j) t, 'g, 'h, 'i, 'j) F.t) ->
            ('b -> 'acc -> 'acc * 'g) ->
            ('c -> 'acc -> 'acc * 'h) ->
            ('d -> 'acc -> 'acc * 'i) ->
            ('e -> 'acc -> 'acc * 'j) ->
            ('deco, 'b, 'c, 'd, 'e) t -> 'acc -> 'acc * ('deco, 'g, 'h, 'i, 'j) t
        end
      val pp : 'a Pp.printer -> 'b Pp.printer -> 'c Pp.printer -> 'd Pp.printer -> ('deco, 'a, 'b, 'c, 'd) t Pp.printer
    end
module FixMut :
  functor (F : Fixables.Fixable4) (G : Fixables.Fixable3) ->
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
      module SurfaceF : Fixables.SigSurface4 with type  ('a, 'b, 'c, 'd) t =  ('a, 'b, 'c, 'd) F.t
      module SurfaceG : Fixables.SigSurface3 with type  ('a, 'b, 'c) t =  ('a, 'b, 'c) G.t
      module Deep :
        sig
          val mapfg_u : ('a -> 'b) -> ('c -> 'd) -> ('deco, 'a, 'c) u -> ('deco, 'b, 'd) u
          val mapfg : ('a -> 'b) -> ('c -> 'd) -> ('deco, 'a, 'c) t -> ('deco, 'b, 'd) t
          val reduce_u : (('acc, 'b, 'c) G.t -> 'acc) -> ('deco, 'b, 'c) u -> 'acc
          val reduce : (('acc, 'acc_u, 'c, 'd) F.t -> 'acc) -> (('acc_u, 'c, 'd) G.t -> 'acc_u) -> ('deco, 'c, 'd) t -> 'acc
          val reduce_u_d : ('deco -> ('acc, 'c, 'd) G.t -> 'acc) -> ('deco, 'c, 'd) u -> 'acc
          val reduce_d : ('deco -> ('b, 'c, 'd, 'e) F.t -> 'b) -> ('deco -> ('c, 'd, 'e) G.t -> 'c) -> ('deco, 'd, 'e) t -> 'b
          val reduce_de : ('deco -> ('b, ('deco, 'c, 'd) u, 'c, 'd) F.t -> 'b) -> ('deco, 'c, 'd) t -> 'b
          val foldmapfg_u : ('a -> 'acc -> 'acc * 'c) -> ('d -> 'acc -> 'acc * 'e) -> ('f, 'a, 'd) u -> 'acc -> 'acc * ('f, 'c, 'e) u
          val foldmapfg : ('a -> 'acc -> 'acc * 'c) -> ('d -> 'acc -> 'acc * 'e) -> ('f, 'a, 'd) t -> 'acc -> 'acc * ('f, 'c, 'e) t
          val map : ((('a, 'b, 'c) t, ('a, 'd, 'e) u, 'd, 'e) F.t -> (('a, 'b, 'c) t, ('a, 'b, 'c) u, 'b, 'c) F.t) -> ('a, 'd, 'e) t -> ('a, 'b, 'c) t
          val decorate_u : ('a -> (('b, 'c, 'd) u, 'c, 'd) G.t -> 'b) -> ('a, 'c, 'd) u -> ('b, 'c, 'd) u
          val decorate : ('a -> (('b, 'c, 'd) t, ('b, 'c, 'd) u, 'c, 'd) F.t -> 'b) -> ('a -> (('b, 'c, 'd) u, 'c, 'd) G.t -> 'b) -> ('a, 'c, 'd) t -> ('b, 'c, 'd) t
          val decorate_foldmap_u : ('a -> (('b, 'c, 'd) u, 'c, 'd) G.t -> 'e -> 'e * 'b) -> ('a, 'c, 'd) u -> 'e -> 'e * ('b, 'c, 'd) u
          val decorate_foldmap : ('a -> (('b, 'c, 'd) t, ('b, 'c, 'd) u, 'c, 'd) F.t -> 'e -> 'e * 'b) -> ('a -> (('b, 'c, 'd) u, 'c, 'd) G.t -> 'e -> 'e * 'b) -> ('a, 'c, 'd) t -> 'e -> 'e * ('b, 'c, 'd) t
        end
      module Traverse :
        sig
          val decorate_foldmap_u :
            ( (('deco_start, 'b, 'c) u -> 'acc -> 'acc * ('deco_end, 'f, 'g) u) ->
              ((('deco_start, 'b, 'c) u, 'b, 'c) G.t -> 'acc -> 'acc * (('deco_end, 'f, 'g) u, 'f, 'g) G.t) ->
              'acc -> 'deco_start -> (('deco_start, 'b, 'c) u, 'b, 'c) G.t ->
              'acc * 'deco_end * (('deco_end, 'f, 'g) u, 'f, 'g) G.t) ->
            ('deco_start -> 'b -> 'acc -> 'acc * 'f) ->
            ('deco_start -> 'c -> 'acc -> 'acc * 'g) ->
            ('deco_start, 'b, 'c) u -> 'acc -> 'acc * ('deco_end, 'f, 'g) u
          val decorate_foldmap :
            ((('deco_begin, 'b, 'c) t -> 'acc -> 'acc * ('deco_end, 'f, 'g) t) ->
             (('deco_begin, 'h, 'i) u -> 'acc -> 'acc * ('j, 'k, 'l) u) ->
             ((('deco_begin, 'b, 'c) t, ('deco_begin, 'h, 'i) u, 'm, 'n) F.t ->
              'acc -> 'acc * (('deco_end, 'f, 'g) t, ('j, 'k, 'l) u, 'o, 'p) F.t) ->
             'acc ->
             'deco_begin ->
             (('deco_begin, 'b, 'c) t, ('deco_begin, 'b, 'c) u, 'b, 'c) F.t -> 'acc * 'deco_end * (('deco_end, 'f, 'g) t, ('deco_end, 'f, 'g) u, 'f, 'g) F.t) ->
            ((('deco_begin, 'h, 'i) u -> 'acc -> 'acc * ('j, 'k, 'l) u) ->
             ((('deco_begin, 'h, 'i) u, 'm, 'n) G.t -> 'acc -> 'acc * (('j, 'k, 'l) u, 'o, 'p) G.t) ->
             'acc -> 'deco_begin -> (('deco_begin, 'h, 'i) u, 'h, 'i) G.t -> 'acc * 'j * (('j, 'k, 'l) u, 'k, 'l) G.t) ->
            ('deco_begin -> 'm -> 'acc -> 'acc * 'o) ->
            ('deco_begin -> 'n -> 'acc -> 'acc * 'p) ->
            ('deco_begin, 'b, 'c) t -> 'acc -> 'acc * ('deco_end, 'f, 'g) t
          val foldmap_u :
            ((('deco, 'b, 'c) u -> 'acc -> 'acc * ('deco, 'e, 'f) u) ->
             ((('deco, 'b, 'c) u, 'g, 'h) G.t -> 'acc -> 'acc * (('deco, 'e, 'f) u, 'i, 'j) G.t) ->
             'acc -> (('deco, 'b, 'c) u, 'b, 'c) G.t -> 'acc * (('deco, 'e, 'f) u, 'e, 'f) G.t) ->
            ('h -> 'acc -> 'acc * 'j) -> ('g -> 'acc -> 'acc * 'i) -> ('deco, 'b, 'c) u -> 'acc -> 'acc * ('deco, 'e, 'f) u
          val foldmap :
            ( (('deco, 'b, 'c) t -> 'acc -> 'acc * ('deco, 'e, 'f) t) ->
              (('deco, 'g, 'h) u -> 'acc -> 'acc * ('deco, 'i, 'j) u) ->
              ((('deco, 'b, 'c) t, ('deco, 'g, 'h) u, 'k, 'l) F.t ->
              'acc -> 'acc * (('deco, 'e, 'f) t, ('deco, 'i, 'j) u, 'm, 'n) F.t) ->
              'acc -> (('deco, 'b, 'c) t, ('deco, 'b, 'c) u, 'b, 'c) F.t ->
              'acc * (('deco, 'e, 'f) t, ('deco, 'e, 'f) u, 'e, 'f) F.t) ->
            ((('deco, 'g, 'h) u -> 'acc -> 'acc * ('deco, 'i, 'j) u) -> ((('deco, 'g, 'h) u, 'k, 'l) G.t -> 'acc -> 'acc * (('deco, 'i, 'j) u, 'm, 'n) G.t) -> 'acc -> (('deco, 'g, 'h) u, 'g, 'h) G.t -> 'acc * (('deco, 'i, 'j) u, 'i, 'j) G.t) ->
            ('k -> 'acc -> 'acc * 'm) -> ('l -> 'acc -> 'acc * 'n) -> ('deco, 'b, 'c) t -> 'acc -> 'acc * ('deco, 'e, 'f) t
        end
      val ppu : 'a Pp.printer -> 'b Pp.printer -> ('deco, 'a, 'b) u Pp.printer
      val pp :  'a Pp.printer -> 'b Pp.printer -> ('deco, 'a, 'b) t Pp.printer
    end
