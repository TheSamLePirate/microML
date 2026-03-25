module Make5 (B:Fixables5.Alignable) (Fixed:Fixables5.Fixed with type ('a, 'b, 'c, 'd, 'e) fixable = ('a, 'b, 'c, 'd, 'd) B.t) : Fixables5.Aligned with type ('a, 'b, 'c, 'd, 'e) fixable = ('a, 'b, 'c, 'd, 'e) B.t and type ('deco, 'b, 'c, 'd, 'e) fixed = ('deco, 'b, 'c, 'd, 'e) Fixed.t = struct

  type ('a, 'b, 'c, 'd, 'e) fixable = ('a, 'b, 'c, 'd, 'e) B.t
  type ('deco, 'b, 'c, 'd, 'e) fixed = ('deco, 'b, 'c, 'd, 'e) Fixed.t

  let eq2fold eq (a, b) acc = acc && eq a b

  let rec same_shape x1 x2 =
    B.align (Fixed.unfix x1) (Fixed.unfix x2)
    |> Option.map (fun couple_t -> Fixed.Surface.fold (fun (a, b) acc -> acc && same_shape a b ) Aux.passacc Aux.passacc Aux.passacc Aux.passacc couple_t true)
    |> Option.value ~default:false

  let rec for_all2  ~on_mismatch prop_b prop_c prop_d prop_e x1 x2 =
    match B.align (Fixed.unfix x1) (Fixed.unfix x2) with
    | None -> on_mismatch x1 x2
    | Some couple_t -> Fixed.Surface.fold
                          (fun (a, b) acc -> acc && for_all2 ~on_mismatch prop_b prop_c prop_d prop_e a b )
                          (eq2fold prop_b) (eq2fold prop_c) (eq2fold prop_d) (eq2fold prop_e) couple_t true

  let equals x = for_all2 ~on_mismatch:(fun _ _ -> false) x

  let rec fold2 ~on_mismatch fold_b fold_c fold_d fold_e (x1, x2) acc =
    let unfixed1 = Fixed.unfix x1
    and unfixed2 = Fixed.unfix x2 in
    match B.align unfixed1 unfixed2 with
      | None -> on_mismatch (x1, x2) acc
      | Some couples_t ->
         Fixed.Surface.fold (fold2 ~on_mismatch fold_b fold_c fold_d fold_e) fold_b fold_c fold_d fold_e couples_t acc
  let fold2 ~on_mismatch fold_b fold_c fold_d fold_e x1 x2 acc = fold2 ~on_mismatch fold_b fold_c fold_d fold_e (x1, x2) acc

  let rec foldmap2_deco ~merge_deco ~on_mismatch foldmap_b foldmap_c foldmap_d foldmap_e (x1, x2) acc =
    let unfixed1 = Fixed.unfix x1
    and unfixed2 = Fixed.unfix x2
    and deco1 = Fixed.decoration x1
    and deco2 = Fixed.decoration x2 in
    let foldmap_b' (a, b) acc = foldmap_b ((deco1, a), (deco2, b)) acc
    and foldmap_c' (a, b) acc = foldmap_c ((deco1, a), (deco2, b)) acc
    and foldmap_d' (a, b) acc = foldmap_d ((deco1, a), (deco2, b)) acc
    and foldmap_e' (a, b) acc = foldmap_e ((deco1, a), (deco2, b)) acc in
    match B.align unfixed1 unfixed2 with
      | None -> on_mismatch (x1, x2) acc
      | Some couples_t ->
         let acc, out = Fixed.Surface.foldmap (foldmap2_deco ~merge_deco ~on_mismatch foldmap_b foldmap_c foldmap_d foldmap_e)
                          foldmap_b' foldmap_c' foldmap_d' foldmap_e' couples_t acc
         in acc, Fixed.fix out (merge_deco deco1 deco2)

  let rec foldmap2 ~on_mismatch foldmap_b foldmap_c foldmap_d foldmap_e (x1, x2) acc =
    let unfixed1 = Fixed.unfix x1
    and unfixed2 = Fixed.unfix x2 in
    match B.align unfixed1 unfixed2 with
      | None -> on_mismatch (x1, x2) acc
      | Some couples_t ->
         let acc, out = Fixed.Surface.foldmap (foldmap2 ~on_mismatch foldmap_b foldmap_c foldmap_d foldmap_e)
                          foldmap_b foldmap_c foldmap_d foldmap_e couples_t acc
         in acc, Fixed.fix out (Fixed.decoration x1)
end

module AlignableConvert3to5 (F : Fixables3.Alignable) : Fixables5.Alignable with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c) F.t = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c) F.t
  let align t1 t2 = F.align t1 t2
end
module AlignableConvert4to5 (F : Fixables4.Alignable) : Fixables5.Alignable with type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd) F.t = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd) F.t
  let align t1 t2 = F.align t1 t2
end

let unit_fm2 _ acc = acc, ()

module Make3 (B:Fixables3.Alignable) (Fixed:Fixables3.Fixed with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) B.t) : Fixables3.Aligned with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) B.t and type ('deco, 'b, 'c) fixed = ('deco, 'b, 'c) Fixed.t = struct
  type ('a, 'b, 'c) fixable = ('a, 'b, 'c) B.t
  type ('deco, 'b, 'c) fixed = ('deco, 'b, 'c) Fixed.t

  module Aligned5 = Make5(AlignableConvert3to5(B))(Fixed.Fixed5)
  let same_shape = Aligned5.same_shape
  let for_all2  ~on_mismatch prop_b prop_c x1 x2 = Aligned5.for_all2 ~on_mismatch prop_b prop_c Aux.forall2 Aux.forall2 x1 x2
  let equals eqb eqc x1 x2 = Aligned5.equals eqb eqc Aux.forall2 Aux.forall2 x1 x2
  let fold2 ~on_mismatch fold_b fold_c x1 x2 acc = Aligned5.fold2 ~on_mismatch fold_b fold_c Aux.passacc Aux.passacc x1 x2 acc
  let foldmap2_deco ~merge_deco ~on_mismatch foldmap_b foldmap_c (x1, x2) acc = Aligned5.foldmap2_deco ~merge_deco ~on_mismatch foldmap_b foldmap_c unit_fm2 unit_fm2 (x1, x2) acc
  let foldmap2 ~on_mismatch foldmap_b foldmap_c (x1, x2) acc = Aligned5.foldmap2 ~on_mismatch foldmap_b foldmap_c unit_fm2 unit_fm2 (x1, x2) acc
end

module Make4 (B:Fixables4.Alignable) (Fixed:Fixables4.Fixed with type ('a, 'b, 'c, 'd) fixable = ('a, 'b, 'c, 'd) B.t) : Fixables4.Aligned with type ('a, 'b, 'c, 'd) fixable = ('a, 'b, 'c, 'd) B.t and type ('deco, 'b, 'c, 'd) fixed = ('deco, 'b, 'c, 'd) Fixed.t = struct
  type ('a, 'b, 'c, 'd) fixable = ('a, 'b, 'c, 'd) B.t
  type ('deco, 'b, 'c, 'd) fixed = ('deco, 'b, 'c, 'd) Fixed.t

  module Aligned5 = Make5(AlignableConvert4to5(B))(Fixed.Fixed5)
  let same_shape = Aligned5.same_shape
  let for_all2  ~on_mismatch prop_b prop_c prop_d x1 x2 = Aligned5.for_all2  ~on_mismatch prop_b prop_c prop_d Aux.forall2 x1 x2
  let equals eqb eqc eqd x1 x2 = Aligned5.equals eqb eqc eqd Aux.forall2 x1 x2
  let fold2 ~on_mismatch fold_b fold_c fold_d x1 x2 acc = Aligned5.fold2 ~on_mismatch fold_b fold_c fold_d Aux.passacc x1 x2 acc
  let foldmap2_deco ~merge_deco ~on_mismatch foldmap_b foldmap_c foldmap_d (x1, x2) acc = Aligned5.foldmap2_deco ~merge_deco ~on_mismatch foldmap_b foldmap_c foldmap_d unit_fm2 (x1, x2) acc
  let foldmap2 ~on_mismatch foldmap_b foldmap_c foldmap_d (x1, x2) acc = Aligned5.foldmap2 ~on_mismatch foldmap_b foldmap_c foldmap_d unit_fm2 (x1, x2) acc

end
