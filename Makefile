build: app/Main.hs
	@cabal build

run: build
	@cabal run
