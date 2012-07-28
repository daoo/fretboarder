warnings = -Wall \
	   -fwarn-incomplete-record-updates \
	   -fwarn-monomorphism-restriction \
	   -fwarn-tabs \
	   -fwarn-unused-do-bind \
	   -fno-warn-orphans \

sources = $(shell find src/ -type f -and -name '*.hs' -and -not -name 'GUI.hs')
flags   = -isrc:build -odir build -hidir build

all: cmdline gui

cmdline: 
	ghc --make $(warnings) $(flags) -o build/fretboarder src/Fretboarder/Applications/CommandLine.hs

gui:
	ghc --make $(warnings) $(flags) -o build/fretboarder-gtk src/Fretboarder/Applications/GUI.hs

ghci:
	ghci $(warnings) $(flags) $(sources)

clean:
	rm -r build/*
	mkdir -p build/Fretboarder/Parser/

lint:
	hlint src -c

.PHONY: clean lint
