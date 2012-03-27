outdir  = build
exename = freboarder

warnings = -Wall \
	   -fno-warn-unused-do-bind \
	   -fwarn-duplicate-exports \
	   -fwarn-hi-shadowing \
	   -fwarn-incomplete-patterns \
	   -fwarn-lazy-unlifted-bindings \
	   -fwarn-missing-fields \
	   -fwarn-missing-methods \
	   -fwarn-missing-signatures \
	   -fwarn-monomorphism-restriction \
	   -fwarn-name-shadowing \
	   -fwarn-orphans \
	   -fwarn-overlapping-patterns \
	   -fwarn-tabs \
	   -fwarn-type-defaults \
	   -fwarn-unused-binds \
	   -fwarn-unused-imports \
	   -fwarn-unused-matches \
	   -fwarn-wrong-do-bind \

sources = $(shell find src/ -type f -name '*.hs')
flags   = -isrc:$(outdir) -odir $(outdir) -hidir $(outdir)

build: lexer parser
	ghc --make $(warnings) $(flags) -o $(outdir)/$(exename) src/Fretboarder/CommandLine/Main.hs

ghci: lexer parser
	ghci $(warnings) $(flags) $(sources)

lexer: src/Fretboarder/Parser/Lexer.x
	alex src/Fretboarder/Parser/Lexer.x -o $(outdir)/Fretboarder/Parser/Lexer.hs

parser: src/Fretboarder/Parser/Parser.y
	happy src/Fretboarder/Parser/Parser.y -o $(outdir)/Fretboarder/Parser/Parser.hs

clean:
	rm -r $(outdir)/*
	mkdir -p $(outdir)/Fretboarder/Parser/

hlint:
	hlint src -c

.PHONY: clean
