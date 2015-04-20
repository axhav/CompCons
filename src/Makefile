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
	-rm -f *.log *.aux *.hi *.o *.dvi *.out
	-rm -f DocJavalette.ps
	-rm -f ../jlc ../lib/*.bc

