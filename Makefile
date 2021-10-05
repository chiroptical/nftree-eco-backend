build: hpack build-lib
	cabal build exe:nftree-eco-backend

build-lib: hpack
	cabal build lib:nftree-eco-backend

build-test: hpack
	cabal build spec

run: build
	cabal run nftree-eco-backend

test: build build-test
	cabal test

hpack:
	hpack .

format-nix:
	nixpkgs-fmt .

format: format-nix
	find src/ app/ test/ -name "*.hs" -exec fourmolu -i {} +
	find src/ app/ test/ -name "*.hs" -exec brittany --indent=4 --write-mode=inplace {} +

ghcid:
	ghcid -c "cabal repl lib:nftree-eco-backend"

ghcid-exe:
	ghcid -c "cabal repl exe:nftree-eco-backend"

clean:
	cabal clean

.PHONY: build build-lib build-test run test hpack format-nix format ghcid ghcid-exe clean
