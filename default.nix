{ compiler ? "ghc8107" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "nftree-eco-backend" =
        hself.callCabal2nix
          "nftree-eco-backend"
          (gitignore ./.)
          { };
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."nftree-eco-backend"
    ];
    buildInputs = [
      # Haskel Development Tools
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.fourmolu
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hpack
      pkgs.haskellPackages.ghcid

      # Utilities
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."nftree-eco-backend");

  docker = pkgs.dockerTools.buildImage {
    name = "nftree-eco-backend";
    config.Cmd = [ "${exe}/bin/nftree-eco-backend" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "nftree-eco-backend" = myHaskellPackages."nftree-eco-backend";
}
