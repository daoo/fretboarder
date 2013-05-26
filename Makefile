build:
	@cabal-dev build --ghc-options="-H64m -rtsopts"

prof:
	@cabal-dev build --ghc-options="-rtsopts -prof -fprof-auto -H64m"

release:
	@cabal-dev build --ghc-options="-fllvm -H64m -O2"

install:
	@cabal-dev install \
		--reinstall \
		--force-reinstalls

conf:
	@cabal-dev configure


conf-tests:
	@cabal-dev configure \
		--enable-benchmarks \
		--enable-tests

test:
	@cabal-dev test

ghci:
	@cabal-dev ghci

clean:
	@cabal-dev clean --save-configure

lint:
	@hlint src
