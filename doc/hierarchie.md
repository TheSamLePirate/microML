# Hiérarchie
dossiers :

- bin/main.ml (* fichier d'entrée qui parse les arguments et lance la compilation*)
- bin/poule.ml (* fichier d'entrée du prouveur *)

- runtime/rt.c (* code C de la runtime *)
- runtime/include/rt.h (* header de la runtime *)

- test/main.t (* fichier cram de tests *)
- test/t%.ml (* fichier de test *)

- lib/lib.ml (* librairie générique pour la gestion d'AST, monades pour récursiver des types paramétrés, et les parcourrir de différentes façons, on a aussi quelques fonctions utilitaires à la lib standard ocaml *)
- lib/ident.ml (* les modules pour les identifiants (variables, fields, etc...) ça délcare des modules avec map, set et fresh *)

- minimllib/ast.ml (* AST primaire, celui fonctionnel, qui sort du parser *)
- minimllib/lexer.mll
- minimllib/parser.mly
- minimllib/typer.ml (* inférence de types *)
- minimllib/bindings.ml (* passe qui donne un nom unique à chaque variable pour gérer le scope *)
- minimllib/inlineBackend.ml (* passe d'inline sur l'AST backend *)
- minimllib/fields.ml (* passe qui donne des noms uniques pour les champs de records et constructeurs *)
- minimllib/lazy.ml (* passe qui permet de rendre lazy a || b et a && b *)
- minimllib/eval.ml (* évaluateur direct sans langage intermédiaire *)
- minimllib/astBackend.ml (* AST bas niveau, et passe de compilation pour la conversion *)
- minimllib/backendCPrinter.ml (* écriture du code C à partir de l'AST final *)
- minimllib/passes.ml (* gestion globale du pipeline des passes *)
- minimlpoulelib/passes.ml (* passes pour le prouveur *)
- poulelib/lexer.mll
- poulelib/parser.mly
- poulelib/types.ml (* définition des types d'AST qui vont être transférés de passe en passe. C'est essentiellement du sucre qui permet d'avoir un nom court à la place de tous les paramètres de types *)
