outdir  = build
exename = freboarder

warnings = -Wall \
	   -fwarn-incomplete-record-updates \
	   -fwarn-monomorphism-restriction \
	   -fwarn-tabs \
	   -fwarn-unused-do-bind \

sources = $(shell find src/ -type f -name '*.hs')
flags   = -isrc:$(outdir) -odir $(outdir) -hidir $(outdir)

build: lexer.o parser.o
	ghc --make $(warnings) $(flags) -o $(outdir)/fretboarder src/Fretboarder/Applications/CommandLine.hs
	ghc --make $(warnings) $(flags) -o $(outdir)/fretboarder-gtk2 src/Fretboarder/Applications/GUI.hs

ghci: lexer.o parser.o
	ghci $(warnings) $(flags) $(sources)

lexer.o: src/Fretboarder/Parser/Lexer.x
	alex src/Fretboarder/Parser/Lexer.x -o $(outdir)/Fretboarder/Parser/Lexer.hs
	ghc --make $(flags) $(outdir)/Fretboarder/Parser/Lexer.hs

parser.o: src/Fretboarder/Parser/Parser.y
	happy src/Fretboarder/Parser/Parser.y -o $(outdir)/Fretboarder/Parser/Parser.hs
	ghc --make $(flags) $(outdir)/Fretboarder/Parser/Parser.hs

clean:
	rm -r $(outdir)/*
	mkdir -p $(outdir)/Fretboarder/Parser/

lint:
	hlint src -c

.PHONY: clean lint
