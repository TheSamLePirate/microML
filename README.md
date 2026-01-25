# MINI OCAML

dossiers :
- bin/main.ml (* fichier d'entrée qui parse les arguments *)
- runtime/rt.c (* code C de la runtime *)
- runtime/rt.h (* header de la runtime *)
- test/t%d.ml (* fichier de test *)
- test/t%d.expected (* fichier de valeurs attendues aux tests *)
- test/run.ml (* fichier pour lancer les tests *)
- lib/lib.ml (* librairie générique pour la gestion d'AST, monades pour récursiver des types paramétrés, et les parcourrir de différentes façons *)
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
build runtime :
```
dune build runtime/libruntime.a
```
compilation d'un test :
```
OCAMLRUNPARAM=b dune exec ./test/run.exe -- test/t02.ml
```

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
