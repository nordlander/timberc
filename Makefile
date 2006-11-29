timberc: *.hs Parser.hs
	ghc -fglasgow-exts -fallow-undecidable-instances -o timberc --make Main

hugs:	*.hs Parser.hs
	hugs -98 -h2500000 Main

Parser.hs: Parser.y
	happy Parser.y

clean:
	rm -f *.o *.hi timberc

