module MakeSurface : functor (F : Fixables4.Fixable) -> Fixables4.SigSurface with type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) F.t

module Fix : functor (F : Fixables4.Fixable) -> Fixables4.Fixed with type ('a, 'b, 'c, 'd) fixable = ('a, 'b, 'c, 'd) F.t
