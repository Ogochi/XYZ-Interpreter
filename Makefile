all:
	happy -gca ParXYZgrammar.y
	alex -g LexXYZgrammar.x
	ghc --make Main.hs -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocXYZgrammar.* LexXYZgrammar.* ParXYZgrammar.* LayoutXYZgrammar.* SkelXYZgrammar.* PrintXYZgrammar.* TestXYZgrammar.* AbsXYZgrammar.* TestXYZgrammar ErrM.* SharedString.* ComposOp.* XYZgrammar.dtd XMLXYZgrammar.* Makefile*
