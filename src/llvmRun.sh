#Runs llvm commands to an output file
#../jlc $1
x=${1%.*}
llvm-as "$x.ll"
llvm-link "$x.bc" ../lib/runtime.bc -o "$x.bc"
llc -filetype=obj "$x.bc" 
gcc "$x.o"
./a.out
#rm -f "$x.o" "$x.bc" "$x.ll"

