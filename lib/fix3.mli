
module MakeSurface : functor (F : Fixables3.Fixable3) -> Fixables3.SigSurface3 with type ('a, 'b, 'c) t = ('a, 'b, 'c) F.t

module Fix : functor (F : Fixables3.Fixable3) -> Fixables3.Fixed3 with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) F.t
