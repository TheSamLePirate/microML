open LibCore.Passes

(*
9. Vérification des toplevels

Entrée : programme complet
Sortie : programme validé

Par déclaration :

TypeDecl : vérifier la déclaration
FunDecl : vérifier la signature
PredDecl : vérifier la signature
Axiom : vérifier la formule
Theorem :
vérifier la formule
vérifier la preuve contre cette formule
Check :
vérifier la formule
vérifier la preuve contre cette formule




. Vérification des preuves
Entrée : proof, formula attendue
Sortie : preuve validée, ou preuve annotée

Jugement central :

￼
Γ ⊢ p : F
Règles typiques :

Var v : chercher v dans le contexte de preuve

Triv : prouve Top

Abort(p, F) : si p : Bottom, alors résultat F

Pair(p1,p2) : prouve A ∧ B

Fst p / Snd p : éliminent A ∧ B

Left / Right : introduisent A ∨ B

Case : élimine A ∨ B

Lam(v, A, p) : introduit A -> B

App(p,q) : élimine A -> B

Gen(v, ty, p) : introduit forall

Inst(p, t) : élimine forall

Pack(ty, t, p) : introduit exists

Unpack(...) : élimine exists

C’est la passe principale de Poule.
*)
let passe = no_pass

