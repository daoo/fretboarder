all:
	@cabal build

clean:
	@cabal clean --save-configure

lint:
	@hlint src -cx

.PHONY: clean lint
