BUILD_DIR := ../build
OUTDIRS := -odir $(BUILD_DIR) -hidir $(BUILD_DIR)

all: Lexer.o Parser.o Fretboarder/CommandLine/Main.hs
	ghc -o $(BUILD_DIR)/fretboarder $(OUTDIRS) \
            -Wall -fwarn-unused-imports \
	    --make Fretboarder/CommandLine/Main.hs

Lexer.o: Lexer
	ghc $(OUTDIRS) Fretboarder/Parser/Lexer.hs

Parser.o: Parser
	ghc $(OUTDIRS) Fretboarder/Parser/Parser.hs

Lexer: Fretboarder/Parser/Lexer.x
	alex Fretboarder/Parser/Lexer.x

Parser: Fretboarder/Parser/Parser.y
	happy Fretboarder/Parser/Parser.y

