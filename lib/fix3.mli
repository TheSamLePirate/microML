
module MakeSurface : functor (F : Fixables3.Fixable) -> Fixables3.SigSurface with type ('a, 'b, 'c) t = ('a, 'b, 'c) F.t
module Fix : functor (F : Fixables3.Fixable) -> Fixables3.Fixed with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) F.t
