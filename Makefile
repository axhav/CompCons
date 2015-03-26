all:
	happy -gca ParJavalette.y
	alex -g LexJavalette.x
	ghc --make TestJavalette.hs -o TestJavalette

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocJavalette.ps

distclean: clean
	-rm -f DocJavalette.* LexJavalette.* ParJavalette.* LayoutJavalette.* SkelJavalette.* PrintJavalette.* TestJavalette.* AbsJavalette.* TestJavalette ErrM.* SharedString.* ComposOp.* Javalette.dtd XMLJavalette.* Makefile*
	

