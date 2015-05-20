all:
	happy -gca ParJavalette.y
	alex -g LexJavalette.x
	ghc -O --make Main.hs -o ../jlc
	llvm-as ../lib/runtime.ll
    
allLocal:
	happy -gca ParJavalette.y
	alex -g LexJavalette.x
	ghc -hide-package mtl -O --make Main.hs -o ../jlc
	llvm-as ../lib/runtime.ll

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.out *.txt *.bc *~
	-rm -f DocJavalette.ps
	-rm -f ../jlc ../lib/*.bc ../lib/*.s ../*.out
	-rm -f ../testsuite/good/*.bc ../testsuite/good/*.ll ../testsuite/good/*.o ../testsuite/good/*.asm
	-rm -f ../testsuite/extensions/arrays1/*.bc ../testsuite/extensions/arrays1/*.ll ../testsuite/extensions/arrays1/*.o ../testsuite/extensions/arrays1/*.asm
	-rm -f ../testsuite/extensions/arrays2/*.bc ../testsuite/extensions/arrays2/*.ll ../testsuite/extensions/arrays2/*.o ../testsuite/extensions/arrays2/*.asm
	-rm -f ../testsuite/extensions/pointers/*.bc ../testsuite/extensions/pointers/*.ll ../testsuite/extensions/pointers/*.o ../testsuite/extensions/pointers/*.asm

