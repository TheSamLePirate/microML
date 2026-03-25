module Make3 (B:Fixables3.Alignable3) (Fixed:Fixables3.Fixed3 with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) B.t) : Fixables3.Aligned3 with type ('a, 'b, 'c) fixable = ('a, 'b, 'c) B.t and type ('deco, 'b, 'c) fixed = ('deco, 'b, 'c) Fixed.t = struct

  type ('a, 'b, 'c) fixable = ('a, 'b, 'c) B.t
  type ('deco, 'b, 'c) fixed = ('deco, 'b, 'c) Fixed.t

  let eq2fold eq (a, b) acc = acc && eq a b

  let rec same_shape x1 x2 =
    B.align (Fixed.unfix x1) (Fixed.unfix x2)
    |> Option.map (fun couple_t -> Fixed.Surface.fold (fun (a, b) acc -> acc && same_shape a b ) Aux.passacc Aux.passacc couple_t true)
    |> Option.value ~default:false

  let rec for_all2  ~on_mismatch prop_b prop_c x1 x2 =
    match B.align (Fixed.unfix x1) (Fixed.unfix x2) with
    | None -> on_mismatch x1 x2
    | Some couple_t -> Fixed.Surface.fold
                          (fun (a, b) acc -> acc && for_all2 ~on_mismatch prop_b prop_c a b )
                          (eq2fold prop_b) (eq2fold prop_c) couple_t true

  let equals eq_b eq_c x1 x2 = for_all2 ~on_mismatch:(fun _ _ -> false) eq_b eq_c x1 x2

  let rec fold2 ~on_mismatch fold_b fold_c (x1, x2) acc =
    let unfixed1 = Fixed.unfix x1
    and unfixed2 = Fixed.unfix x2 in
    match B.align unfixed1 unfixed2 with
      | None -> on_mismatch (x1, x2) acc
      | Some couples_t ->
         Fixed.Surface.fold (fold2 ~on_mismatch fold_b fold_c) fold_b fold_c couples_t acc
  let fold2 ~on_mismatch fold_b fold_c x1 x2 acc = fold2 ~on_mismatch fold_b fold_c (x1, x2) acc

  let rec foldmap2_deco ~merge_deco ~on_mismatch foldmap_b foldmap_c (x1, x2) acc =
    let unfixed1 = Fixed.unfix x1
    and unfixed2 = Fixed.unfix x2
    and deco1 = Fixed.decoration x1
    and deco2 = Fixed.decoration x2 in
    let foldmap_b' (a, b) acc = foldmap_b ((deco1, a), (deco2, b)) acc
    and foldmap_c' (a, b) acc = foldmap_c ((deco1, a), (deco2, b)) acc in
    match B.align unfixed1 unfixed2 with
      | None -> on_mismatch (x1, x2) acc
      | Some couples_t ->
         let acc, out = Fixed.Surface.foldmap (foldmap2_deco ~merge_deco ~on_mismatch foldmap_b foldmap_c)
                          foldmap_b' foldmap_c' couples_t acc
         in acc, Fixed.fix out (merge_deco deco1 deco2)

  let rec foldmap2 ~on_mismatch foldmap_b foldmap_c (x1, x2) acc =
    let unfixed1 = Fixed.unfix x1
    and unfixed2 = Fixed.unfix x2 in
    match B.align unfixed1 unfixed2 with
      | None -> on_mismatch (x1, x2) acc
      | Some couples_t ->
         let acc, out = Fixed.Surface.foldmap (foldmap2 ~on_mismatch foldmap_b foldmap_c)
                          foldmap_b foldmap_c couples_t acc
         in acc, Fixed.fix out (Fixed.decoration x1)
end

