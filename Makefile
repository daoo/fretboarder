build:
	@cabal build --ghc-options="-rtsopts"

prof:
	@cabal configure --ghc-options="-Wall" --enable-library-profiling --enable-executable-profiling
	@cabal build --ghc-options="-rtsopts -prof -fprof-auto"
	@cabal configure --ghc-options="-Wall" --disable-library-profiling --disable-executable-profiling

release:
	@cabal build --ghc-options="-fllvm -O2"

init:
	@cabal sandbox init
	@cabal install --only-dependencies --enable-library-profiling --enable-tests --enable-benchmarks
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
