module Make3 (B:Fixables3.Comparable3) (Fixed:Fixables3.Fixed3 with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) B.t) : Fixables3.Comparor3 with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) B.t and type ('deco, 'b, 'c) fixed = ('deco, 'b, 'c) Fixed.t = struct

  type ('a, 'b, 'c) fixable = ('a, 'b, 'c) B.t
  type ('deco, 'b, 'c) fixed = ('deco, 'b, 'c) Fixed.t

  let cmp2fold cmp (a, b) acc = if acc = 0 then cmp a b else acc

  let rec compare cmp_b cmp_c (x1, x2) acc =
    if acc = 0 then
      let unfixed1 = Fixed.unfix x1
      and unfixed2 = Fixed.unfix x2 in
      match B.align unfixed1 unfixed2 with
      | None -> B.tag unfixed1 - B.tag unfixed2
      | Some couples_t ->
         Fixed.Surface.fold (compare cmp_b cmp_c) cmp_b cmp_c couples_t 0
    else acc

  let compare cmp_b cmp_c x1 x2 = compare (cmp2fold cmp_b) (cmp2fold cmp_c) (x1, x2) 0

end
