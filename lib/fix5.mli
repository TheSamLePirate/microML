module MakeSurface : functor (F : Fixables5.Fixable) -> Fixables5.SigSurface with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) F.t
module Fix : functor (F : Fixables5.Fixable) -> Fixables5.Fixed with type ('a, 'b, 'c, 'd, 'e) fixable = ('a, 'b, 'c, 'd, 'e) F.t
