BUILD_DIR := ../build
OUTDIRS := -odir $(BUILD_DIR) -hidir $(BUILD_DIR)
GHC_WARNINGS := -fno-warn-unused-do-bind \
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
		-fwarn-wrong-do-bind

all: Lexer.o Parser.o Fretboarder/CommandLine/Main.hs
	ghc -o $(BUILD_DIR)/fretboarder $(OUTDIRS) \
            -Wall $(GHC_WARNINGS) \
	    --make Fretboarder/CommandLine/Main.hs

Lexer.o: Lexer
	ghc $(OUTDIRS) Fretboarder/Parser/Lexer.hs

Parser.o: Parser
	ghc $(OUTDIRS) Fretboarder/Parser/Parser.hs

Lexer: Fretboarder/Parser/Lexer.x
	alex Fretboarder/Parser/Lexer.x

Parser: Fretboarder/Parser/Parser.y
	happy Fretboarder/Parser/Parser.y

