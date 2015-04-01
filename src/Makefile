all:
	happy -gca ParJavalette.y
	alex -g LexJavalette.x
	ghc -O --make Main.hs -o ../jlc
    
allLocal:
	happy -gca ParJavalette.y
	alex -g LexJavalette.x
	ghc -hide-package mtl -O --make Main.hs -o ../jlc

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocJavalette.ps
	-rm -f ../jlc

