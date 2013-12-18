build:
	@cabal build --ghc-options="-H64m -rtsopts"

prof:
	@cabal build --ghc-options="-rtsopts -prof -fprof-auto -H64m"

release:
	@cabal build --ghc-options="-fllvm -H64m -O2"

init:
	@cabal sandbox init
	@cabal install --only-dependencies --enable-tests --enable-benchmarks
	@cabal configure --ghc-options="-Wall" --disable-tests --disable-benchmarks

test:
	@cabal configure --ghc-options="-Wall" --enable-tests
	@cabal build
	./dist/build/tests/tests
	@cabal configure --ghc-options="-Wall" --disable-tests --disable-benchmarks

ghci:
	ghci -isrc -package-db=./.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d/ src/Main.hs

clean:
	@cabal clean --save-configure

tags:
	@cabal repl :ctags

lint:
	@hlint src
