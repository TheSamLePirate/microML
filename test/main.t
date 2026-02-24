  $ buildandrun(){
  > exe="${1%.*}.exe"
  > ../bin/main.exe --runtime ../runtime/libruntime.a "$1" -o "$exe" -I ../runtime/include > /dev/null 2>&1 || true
  > ./$exe
  > }

TESTING MAIN
  $ ../bin/main.exe --help
  usage: microml <file>
    -o output file
    --runtime runtime file
    -I runtime header directory
    -help  Display this list of options
    --help  Display this list of options
test 1
  $ buildandrun t01.ml
  2
test 2
  $ buildandrun t02.ml
  2
  $ buildandrun t03.ml
  2
