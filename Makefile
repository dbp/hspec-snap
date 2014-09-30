.PHONY: test

test:
	cabal exec -- runghc -isrc spec/Main.hs
