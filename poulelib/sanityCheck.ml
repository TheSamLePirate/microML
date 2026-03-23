open LibCore.Passes
(* TODO
À vérifier :

pas de doublons de noms globaux
arité correcte des types paramétrés
constructeurs référencent des types existants
signatures de fonctions bien formées
signatures de prédicats bien formées
chaque variable de type utilisée est bien liée
pas de constructeur déclaré deux fois dans le même type
5. Vérification de bonne formation des types

Entrée : Ty.t
Sortie : Ty.t validé, ou version annotée

À vérifier :

TyVar bien liée
TyNamed(name, args) a la bonne arité
TyArrow(args, ret) contient des types valides partout

Cette passe peut être appelée par d’autres :

vérification des déclarations
vérification des formules
vérification des preuves
6. Vérification de bonne formation des termes

Entrée : Term.t
Sortie : terme bien formé, idéalement annoté par son type

À vérifier :

variable liée ou globale autorisée selon le contexte
Constr(c, args) : constructeur connu, bon nombre d’arguments
App(f, args) : fonction connue, bon nombre d’arguments
types des arguments compatibles

Sortie utile :

soit simple validation
soit term * ty

Cette passe est centrale si tes formules contiennent des termes.

7. Vérification de bonne formation des formules

Entrée : Formula.t
Sortie : formule bien formée

À vérifier :

Pred(p, li) : prédicat déclaré, bonne arité, arguments bien typés
Eq(t1, t2) : les deux termes ont le même type
connecteurs logiques bien formés
Forall(v, ty, f) : type valide, variable liée dans f
Exists(v, ty, f) : idem

But :

garantir que les énoncés sont sensés avant de regarder les preuves
*)
let passe = no_pass

