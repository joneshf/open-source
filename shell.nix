{ compiler ? "ghc822" }:

let

  default = haskell-packages.callPackage (import ./default.nix) {};

  derivation = nixpkgs.haskell.lib.addBuildTools default tools;

  haskell-packages = nixpkgs.haskell.packages.${compiler};

  nixpkgs = import nixpkgs-tarball {};

  nixpkgs-revision = "120b013e0c082d58a5712cde0a7371ae8b25a601";

  nixpkgs-sha256 = "0hk4y2vkgm1qadpsm4b0q1vxq889jhxzjx3ragybrlwwg54mzp4f";

  nixpkgs-tarball = builtins.fetchTarball {
    sha256 = nixpkgs-sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgs-revision}.tar.gz";
  };

  # Tools affected by the compiler version.
  tools-compiler = [
    haskell-packages.cabal-install
    haskell-packages.ghc
    haskell-packages.ghcid
    haskell-packages.hpack
  ];

  # Tools that deal with the basic project infrastructure.
  tools-infrastructure = [
    nixpkgs.cabal2nix
    nixpkgs.curl
    nixpkgs.which
  ];

  # All of the tools.
  tools =
    tools-compiler ++
    tools-infrastructure;

in

  derivation.env
