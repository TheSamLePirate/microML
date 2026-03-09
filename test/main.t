  $ buildandrun(){
  > exe="${1%.*}.exe"
  > ../bin/main.exe --runtime ../runtime/libruntime.a "$1" -o "$exe" -I ../runtime/include > /dev/null 2>&1 || true
  > ./$exe
  > }

  $ nocompile(){
  > ../bin/main.exe --runtime ../runtime/libruntime.a "$1" -o "$exe" -I ../runtime/include > /dev/null 2>&1
  > echo $?
  > }

TESTING MAIN
  $ ../bin/main.exe --help
  usage: microml <file>
    --debug add debug (args must be parsing, bindings, fields, eval or backend)
    -o output file
    --runtime runtime file
    -I runtime header directory
    -help  Display this list of options
    --help  Display this list of options
compile pas :
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
  $ buildandrun t_construct.ml
  12
  $ buildandrun t_constructtuple.ml
  2
  $ buildandrun t_polymorphisme.ml
tests randoms :
  $ buildandrun t01.ml
  2
  $ buildandrun t02.ml
  2
  $ buildandrun t03.ml
  234
  $ buildandrun t04.ml
  43210
  $ buildandrun t05.ml
  6
