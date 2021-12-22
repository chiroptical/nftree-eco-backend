build: hpack build-lib
	cabal build exe:server

build-lib: hpack
	cabal build lib:nftree-eco-backend

build-seed: hpack
	cabal build exe:seed

build-test: hpack
	cabal build spec

run: build
	cabal run exe:server

run-seed: build-seed
	cabal run exe:seed

test: build build-test
	cabal test

hpack:
	hpack .

format-nix:
	nixpkgs-fmt .

format: format-nix
	find src/ app/ test/ seed/ -name "*.hs" -exec fourmolu -i -o '-XTypeApplications' -o '-XImportQualifiedPost' {} +

repl:
	cabal repl lib:nftree-eco-backend

ghcid:
	ghcid -c "cabal repl lib:nftree-eco-backend"

ghcid-server:
	ghcid -c "cabal repl exe:server"

ghcid-seed:
	ghcid -c "cabal repl exe:seed"

clean:
	cabal clean

.PHONY: build build-lib build-test run test hpack format-nix format ghcid ghcid-exe clean build-seed run-seed repl
