# MINI OCAML

Pour ceux qui ne connaissent pas ocaml, il faut installer opam, le configurer, installer dune, menhir, et coté IDE, j'ai codé sous emacs avec merlin.

Le projet permet de parser, typer, évaluer, compiler un micro-caml très simple. C'est inspiré du projet étudant d'un ami, mais son projet à lui restait très primitif. Je suis donc parti sur un micro-caml qui permet du polymorphisme, des types sommes, du pattern matching, avec un backend llvm.

dossiers :
- bin/main.ml (* fichier d'entrée qui parse les arguments et lance la compilation*)
- runtime/rt.c (* code C de la runtime *)
- runtime/include/rt.h (* header de la runtime *)
- test/main.t (* fichier cram de tests *)
- test/t%d.ml (* fichier de test *)
- test/t%d.expected (* fichier de valeurs attendues aux tests *)
- test/run.ml (* fichier pour lancer les tests *)
- lib/lib.ml (* librairie générique pour la gestion d'AST, monades pour récursiver des types paramétrés, et les parcourrir de différentes façons, on a aussi quelques fonctions utilitaires à la lib standard ocaml *)
- lib/ast.ml (* AST primaire, celui fonctionnel, qui sort du parser *)
- lib/lexer.mll
- lib/parser.mly
- lib/typer.ml (* inférence de types *)
- lib/bindings.ml (* passe qui donne un nom unique à chaque variable pour gérer le scope *)
- lib/eval.ml (* évaluateur direct sans langage intermédiaire *)
- lib/astBackend.ml (* AST bas niveau, et passe de compilation pour la conversion *)
- lib/backendCPrinter.ml (* écriture du code C à partir de l'AST final *)
- lib/passes.ml (* gestion globale du pipeline des passes *)

QUICK START
build runtime (normalement on a pas besoin de ça) :
```
dune build runtime/libruntime.a
```
tests :
```
dune runtest
```
test spécifique :
```
dune runtest test/main.t
```
Usage :
```
OCAMLRUNPARAM=b dune exec -- bin/main.exe test/t02.ml
```

Ajout de l'option --debug <nom de la passe> pour pouvoir afficher un AST intermédiaire (utile pour le debug)

## Lib
module Pp : utilitaires pour les Printers
module Applicative (map, fold, foldmap, accumule) : opérateurs de parcours de type récursif
module ListApp : un fonctor pour parcourrir une liste avec une applicative
module type Fixable3 : AST récursif, avec 3 variables (dont une pour la récursion)
module type Fixable4 : AST récursif, avec 3 variables (dont une pour la récursion)
module Fix3 (F : Fixable3) et
module Fix4 (F : Fixable4) : génère le type fixé, et les méthodes de parcours en surface ou en profondeur
module FixMut  (F : Fixable4) (G : Fixable3) : Quand un type Fixbale4 a comme variable secondaire, un Fixable3.

Les Fix3, Fix4 et FixMut génèrent des sous modules : Surface, Deep, Traverse, et une fonction pp.
les types fixés ont un décorateur, ce qui permet d'accrocher une information (type, bindingMap, etc...) à chaque noeud.

Ces trois functors permettent de n'avoir quasiment rien à écrire pour chaque AST (on a un AST de type, de patterns, d'expressions, et de backend bas niveau) : on écrit un pretty printer (avec le module Format) et une méthode de parcours en surface par une Applicative, et c'est tout. Ensuite, ces functors génèrent les mêmes méthodes de parcours pour chaque AST. Grosse factorisation de code.

Les modules Fix permettent d'ajouter un polymorphisme supplémentaire à chaque noeud, c'est ce qu'on nomme une décoration. Le parseur produit un AST décoré par des locations (les positions dans les fichiers) ensuite, cette décoration change : le typeur permet de la transformer en location * type. Ainsi chaque expression est reliée à un type. L'alternative à cette solution est d'avoir un int qui se colle à chaque noeud, et une intmap produite par le typer. Le problème avec cette méthode c'est qu'on perd les garanties : on est plus certain qu'un type est sur une expression. Par contre on gagne en souplesse, on peut créer des noeuds sans les typer, ou réordonner des passes. Mais le typage pour moi est une garantie importante.

Grâce à ce module, la plupart des passes de compilation sont soit des traverse, soit des reduce, soit des foldmap. Fixed.Deep est toujours utilisé pour lancer une réécriture.

TODO
patterns :
 - vérification de patterns inutiles
 - vérification de patterns non exhaustifs
