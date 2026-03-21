module Pp = LibCore.Pp
module type Applicative = LibCore.Applicatives.Applicative
module Fix3 = LibCore.Lib.Fix3
module Fix4 = LibCore.Lib.Fix4
module Fix5 = LibCore.Lib.Fix5
module ListApp = LibCore.Applicatives.ListApp
module Ident = LibCore.Ident

module Term = struct
  module Fixable = struct
    type ('term, 'var, 'constr, 'funsym) t =
      | Var of 'var
      | Constr of 'constr * 'term list
      | App of 'funsym * 'term list
    let pp ppterm ppvar ppconstr ppfunsym f = function
      | Var v -> ppvar f v
      | Constr (c, li) -> Format.fprintf f "%a(%a)" ppconstr c (Pp.list (Pp.sep ", ") ppterm) li
      | App (funsym, li) -> Format.fprintf f "%a(%a)" ppfunsym funsym (Pp.list (Pp.sep ", ") ppterm) li
    module Make(F:Applicative) = struct
      open F
      include ListApp(F)
      let foldmap f g h i = function
        | Var v -> ret (fun v -> Var v) <*> g v
        | Constr (constr, li) -> ret (fun constr li -> Constr(constr, li)) <*> h constr <*> fold_left_map f li
        | App (funsym, li) -> ret (fun funsym li -> App(funsym, li)) <*> i funsym <*> fold_left_map f li
    end
  end
  module Fixed = Fix4(Fixable)
  let fix loc node = Fixed.fix node loc
end

module Ty = struct
  module Fixable = struct
    type ('a, 'tyname, 'tyvar) t =
      | TyArrow of 'a list * 'a
      | TyVar of 'tyvar
      | TyNamed of 'tyname * 'a list
    let pp ppa pptyname pptyvar f = function
      | TyArrow (li, a) -> Format.fprintf f "(%a) -> %a" (Pp.list (Pp.sep ", ") ppa) li ppa a
      | TyVar tv -> pptyvar f tv
      | TyNamed (name, li) -> Format.fprintf f "%a(%a)" pptyname name (Pp.list (Pp.sep ", ") ppa) li
    module Make(F:Applicative) = struct
      open F
      include ListApp(F)
      let foldmap f g h = function
          | TyArrow (li, a) -> ret (fun li a -> TyArrow(li, a)) <*> fold_left_map f li <*> f a
          | TyVar var -> ret (fun var -> TyVar var) <*> h var
          | TyNamed (name, li) -> ret (fun name li -> TyNamed (name, li)) <*> g name <*> fold_left_map f li
    end
  end
  module Fixed = Fix3(Fixable)
  let fix loc node = Fixed.fix node loc
end

module Formula = struct
  module Fixable = struct
    type ('formula, 'ty, 'var, 'term, 'pred) t =
      | Top
      | Bottom
      | Pred of 'pred * 'term list
      | Eq of 'term * 'term
      | And of 'formula * 'formula
      | Or of 'formula * 'formula
      | Impl of 'formula * 'formula
      | Forall of 'var * 'ty * 'formula
      | Exists of 'var * 'ty * 'formula
    let pp ppf ppty ppvar ppterm pppred f = function
      | Top -> Format.fprintf f "Top"
      | Bottom -> Format.fprintf f "Bottom"
      | Pred (p, li) -> Format.fprintf f "%a(%a)" pppred p (Pp.list (Pp.sep ", ") ppterm) li
      | Eq (t1, t2) -> Format.fprintf f "%a = %a" ppterm t1 ppterm t2
      | And (f1, f2) -> Format.fprintf f "%a ∧ %a" ppf f1 ppf f2
      | Or (f1, f2) -> Format.fprintf f "%a ∨ %a" ppf f1 ppf f2
      | Impl (f1, f2) -> Format.fprintf f "%a ⇒ %a" ppf f1 ppf f2
      | Forall (v, ty, f') -> Format.fprintf f "∀ %a : %a . %a" ppvar v ppty ty ppf f'
      | Exists (v, ty, f') -> Format.fprintf f "∃ %a : %a . %a" ppvar v ppty ty ppf f'
    module Make(F:Applicative) = struct
      open F
      include ListApp(F)
      let foldmap f g h i j = function
          | Top -> ret Top
          | Bottom ->  ret Bottom
          | Pred (p, li) -> ret (fun p li -> Pred (p, li)) <*> j p <*> fold_left_map i li
          | Eq (a, b) -> ret (fun a b -> Eq (a, b)) <*> i a <*> i b
          | And (a, b) -> ret (fun a b -> And (a, b)) <*> f a <*> f b
          | Or (a, b) -> ret (fun a b -> Or (a, b)) <*> f a <*> f b
          | Impl (a, b) -> ret (fun a b -> Impl (a, b)) <*> f a <*> f b
          | Forall (v, ty, form) -> ret (fun v ty f -> Forall (v, ty, f)) <*> h v <*> g ty <*> f form
          | Exists (v, ty, form) -> ret (fun v ty f -> Exists (v, ty, f)) <*> h v <*> g ty <*> f form
    end
  end
  module Fixed = Fix5(Fixable)
  let fix loc node = Fixed.fix node loc
end
module ProofTerm = struct
  module Fixable = struct
    type ('self, 'formula, 'ty, 'term, 'var) t =
      | Var of 'var

      | Triv (* top et bottom *)
      | Abort of 'self * 'formula

      | Pair of 'self * 'self
      | Fst of 'self
      | Snd of 'self

      | Left of 'self * 'formula
      | Right of 'formula * 'self
      | Case of 'self * ('var * 'self) * ('var * 'self)

      | Lam of 'var * 'formula * 'self
      | App of 'self * 'self

      | Gen of 'var * 'ty * 'self
      | Inst of 'self * 'term

      | Pack of 'ty * 'term * 'self
      | Unpack of 'term * 'var * 'var * 'self

    let pp pp pf pty pterm pvar f = function
      | Var v -> pvar f v
      | Triv -> Format.fprintf f "Triv"

      | Abort (s, f') ->  Format.fprintf f "abort(%a, %a)" pp s pf f'
      | Pair (a, b) -> Format.fprintf f "(%a, %a)" pp a pp b

      | Fst a -> Format.fprintf f "(fst %a)" pp a
      | Snd a -> Format.fprintf f "(snd %a)" pp a

      | Left (p, f') -> Format.fprintf f "(left %a %a)" pp p pf f'
      | Right (f', p) -> Format.fprintf f "(left %a %a)" pf f' pp p

      | Case (f', (v1, f1), (v2, f2)) -> Format.fprintf f "(case %a (%a %a) (%a %a))" pp f' pvar v1 pp f1 pvar v2 pp f2

      | Lam (v, f', s) -> Format.fprintf f "(λ %a : %a . %a)" pvar v pf f' pp s
      | App (s1, s2) -> Format.fprintf f "(%a)(%a)" pp s1 pp s2

      | Gen (v, ty, s) -> Format.fprintf f "gen(%a, %a, %a)" pvar v pty ty pp s
      | Inst (s, t) -> Format.fprintf f "inst(%a, %a)" pp s pterm t

      | Pack (ty, t, s) -> Format.fprintf f "pack(%a, %a, %a)" pty ty pterm t pp s
      | Unpack (t, v1, v2, s) -> Format.fprintf f "unpack(%a, %a, %a, %a)" pterm t pvar v1 pvar v2 pp s
    module Make(F:Applicative) = struct
      open F
      include ListApp(F)
      let foldmap f g h i j = function
          | Var v -> ret (fun v -> Var v) <*> j v
          | Triv -> ret Triv
          | Abort (s, form) -> ret (fun s f -> Abort(s, f)) <*> f s <*> g form
          | Pair (s1, s2) -> ret (fun s1 s2 -> Pair(s1, s2)) <*> f s1 <*> f s2
          | Fst s -> ret (fun s -> Fst s) <*> f s
          | Snd s -> ret (fun s -> Snd s) <*> f s
          | Left (s, form) -> ret (fun s form -> Left(s, form)) <*> f s <*> g form
          | Right (form, s) -> ret (fun form s -> Right(form, s)) <*> g form <*> f s
          | Case (s, (v1, s1), (v2, s2)) -> ret (fun s v1 s1 v2 s2 -> Case (s, (v1, s1), (v2, s2))) <*> f s <*> j v1 <*> f s1 <*> j v2 <*> f s2
          | Lam (v, form, s) -> ret (fun v form s -> Lam(v, form, s)) <*> j v <*> g form <*> f s
          | App (s1, s2) -> ret (fun s1 s2 -> App(s1, s2)) <*> f s1 <*> f s2
          | Gen (v, ty, s) -> ret (fun v ty s -> Gen(v, ty, s)) <*> j v <*> h ty <*> f s
          | Inst (s, t) -> ret (fun s t -> Inst( s, t)) <*> f s <*> i t
          | Pack (ty, t, s) -> ret (fun ty t s -> Pack (ty, t, s)) <*> h ty <*> i t <*> f s
          | Unpack (t, v1, v2, s) -> ret (fun t v1 v2 s -> Unpack (t, v1, v2, s)) <*> i t <*> j v1 <*> j v2 <*> f s
    end

  end
  module Fixed = Fix5(Fixable)
  let fix loc node = Fixed.fix node loc
end

module TopLevel = struct
  type ('ty, 'term, 'formula, 'proof, 'name) t =
    | TypeDecl of {
        name : 'name;
        params : 'name list;
        constructors : ('name * 'ty list) list;
      }
    | FunDecl of {
        name : 'name;
        args : 'ty list;
        ret : 'ty;
      }
    | PredDecl of {
        name : 'name;
        args : 'ty list;
      }
    | Axiom of {
        name : 'name;
        formula : 'formula;
      }
    | Theorem of {
        name : 'name;
        formula : 'formula;
        proof : 'proof;
      }
    | Check of {
        proof : 'proof;
        formula : 'formula;
      }
end

module Prog
         (V : Ident.Bindings)
         (C : Ident.Fields)
         (F : Ident.Bindings)
         (A : sig type deco type pred end)
  = struct
  module Var = V
  module Constr = C
  module Funsym = F
  type deco = A.deco
  type pred = A.pred
  type ty = (deco, Funsym.t, Var.t) Ty.Fixed.t
  type term = (deco, Var.t, Constr.t, Funsym.t) Term.Fixed.t
  type formula = (deco, ty, Var.t, term, pred) Formula.Fixed.t
  type proof = (deco, formula, ty, term, Var.t) ProofTerm.Fixed.t
end
