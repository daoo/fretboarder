warnings = -Wall \
	   -fwarn-incomplete-record-updates \
	   -fwarn-monomorphism-restriction \
	   -fwarn-tabs \
	   -fwarn-unused-do-bind \
	   -fno-warn-orphans \

sources = $(shell find src/ -type f -and -name '*.hs' -and -not -name 'GUI.hs')
flags   = -isrc:build -odir build -hidir build

all: lexer parser cmdline gui

cmdline: 
	ghc --make $(warnings) $(flags) -o build/fretboarder src/Fretboarder/Applications/CommandLine.hs

gui:
	ghc --make $(warnings) $(flags) -o build/fretboarder-gtk src/Fretboarder/Applications/GUI.hs

ghci: lexer_o parser_o
	ghci $(warnings) $(flags) $(sources)

lexer: lexer_hs lexer_o

parser: parser_hs parser_o

lexer_hs: src/Fretboarder/Parser/Lexer.x
	alex src/Fretboarder/Parser/Lexer.x -o build/Fretboarder/Parser/Lexer.hs

lexer_o: build/Fretboarder/Parser/Lexer.hs
	ghc --make $(flags) build/Fretboarder/Parser/Lexer.hs

parser_hs: src/Fretboarder/Parser/Parser.y
	happy -agc src/Fretboarder/Parser/Parser.y -o build/Fretboarder/Parser/Parser.hs

parser_o: build/Fretboarder/Parser/Parser.hs
	ghc --make $(flags) build/Fretboarder/Parser/Parser.hs

clean:
	rm -r build/*
	mkdir -p build/Fretboarder/Parser/

lint:
	hlint src -c

.PHONY: clean lint
