BUILD_DIR=../build

all: lexer parser Fretboarder/CommandLine/Main.hs
	ghc -o $(BUILD_DIR)/fretboarder -odir $(BUILD_DIR)/ -hidir $(BUILD_DIR)/ --make Fretboarder/CommandLine/Main.hs

lexer: Fretboarder/Parser/Lexer.x
	alex Fretboarder/Parser/Lexer.x

parser: Fretboarder/Parser/Parser.y
	happy Fretboarder/Parser/Parser.y

