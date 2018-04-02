all:
	bnfc -haskell Cascal.cf
	happy -gca ParCascal.y
	alex -g LexCascal.x
	ghc --make Main.hs -o Interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocCascal.* LexCascal.* ParCascal.* LayoutCascal.* SkelCascal.* PrintCascal.* TestCascal.* AbsCascal.* TestCascal ErrM.* SharedString.* ComposOp.* Cascal.dtd XMLCascal.* Interpreter
	

