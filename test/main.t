  $ buildandrun(){
  > exe="${1%.*}.exe"
  > ../bin/main.exe --runtime ../runtime/libruntime.a "$1" -o "$exe" -I ../runtime/include > /dev/null 2>&1 || true
  > ./$exe
  > }
  $ evalme(){
  > ../bin/main.exe "$1" --eval
  > }
  $ nocompile(){
  > ../bin/main.exe --runtime ../runtime/libruntime.a "$1" -o "$exe" -I ../runtime/include > /dev/null 2>&1
  > echo $?
  > }

TESTING MAIN
  $ ../bin/main.exe --help
  usage: microml <file>
    --eval mode évaluation
    --debug add debug (args must be parsing, bindings, fields, eval or backend)
    -o output file
    --runtime runtime file
    -I runtime header directory
    -help  Display this list of options
    --help  Display this list of options
compile pas :
  $ nocompile t_bindings.ml
  2
  $ nocompile t_notype0.ml
  2
  $ nocompile t_notype1.ml
  2
  $ nocompile t_notype2.ml
  2
  $ nocompile t_notype3.ml
  2
  $ nocompile t_notype4.ml
  2
tests de features :
  $ buildandrun t_tuple.ml
  12
  $ evalme t_tuple.ml
  12
  $ buildandrun t_construct.ml
  12
  $ evalme t_construct.ml
  12
  $ buildandrun t_constructtuple.ml
  2
  $ evalme t_constructtuple.ml
  2
  $ buildandrun t_polymorphisme.ml
  $ evalme t_polymorphisme.ml
tests randoms :
  $ buildandrun t01.ml
  2
  $ evalme t01.ml
  2
  $ buildandrun t02.ml
  2
  $ evalme t02.ml
  2
  $ buildandrun t03.ml
  234
  $ evalme t03.ml
  234
  $ buildandrun t04.ml
  43210
  $ evalme t04.ml
  43210
  $ buildandrun t05.ml
  6
  $ evalme t05.ml
  6

  $ buildandrun t06.ml
  42115821441104100115999922120
  $ evalme t06.ml
  42115821441104100115999922120


  $ buildandrun t_lazy.ml
  11
  $ evalme t_lazy.ml
  11

  $ buildandrun t_lazy1.ml
  11
  $ evalme t_lazy1.ml
  11
