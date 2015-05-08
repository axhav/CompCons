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
	-rm -f ../jlc ../lib/*.bc ../*.out
	-rm -f ../testsuite/good/*.bc ../testsuite/good/*.ll ../testsuite/good/*.o
	-rm -f ../testsuite/extensions/arrays1/*.bc ../testsuite/extensions/arrays1/*.ll ../testsuite/extensions/arrays1/*.o

