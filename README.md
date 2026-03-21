# MINI OCAML

Le projet permet de parser, typer, évaluer, compiler un micro-caml très simple. C'est inspiré du projet étudant d'un ami, mais son projet à lui restait très primitif. Je suis donc parti sur un micro-caml qui permet les fonctionnalités suivantes :
- un typer avec du polymorphisme
- des types sommes
- des tuples
- des clotures (applications partielles, déclarations de fonctions avec un environement)
- du pattern matching
- un backend en C, avec pour objectif ultime un backend llvm.
et quelques passes de compilations additionnelles :
- transformation des variables et champs en entiers
- && et || lazy
- de l'inline sur les variables du backend

# Dépendances
Pour ceux qui ne connaissent pas ocaml, il faut installer opam, le configurer, installer dune, menhir, et coté IDE, j'ai codé sous emacs avec merlin. Pour certains trucs, j'ai besoin d'ocaml-print-intf mais seulement pour de la doc.

# QUICK START
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

Ajout de l'option --debug <nom de la passe> pour pouvoir afficher un AST intermédiaire (utile pour le debug. Les passes dispo sont dans le fichier lib/passes.ml)
