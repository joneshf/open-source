# The basic setup for the repo.
# We can have these tools everywhere in the repo,
# while also keeping track of them for the future.

let

  buildInputs =
    [ haskell-packages ] ++
    tools;

  haskell-packages = nixpkgs.haskellPackages.ghcWithHoogle (p:
    haskell-packages-katip-rollbar p ++
    haskell-packages-rollbar-hs p ++
    haskell-packages-shake p ++
    haskell-packages-wai-middleware-rollbar p
  );

  haskell-packages-katip-rollbar = p: [
    p.aeson
    p.async
    p.base
    p.hostname
    p.http-client
    p.katip
    p.rollbar-hs
    p.stm-chans
    p.text
    p.time
  ];

  haskell-packages-rollbar-hs = p: [
    p.QuickCheck
    p.aeson
    p.base
    p.bytestring
    p.case-insensitive
    p.hostname
    p.hspec
    p.hspec-golden-aeson
    p.http-client
    p.http-conduit
    p.http-types
    p.network
    p.text
    p.time
    p.unordered-containers
    p.uuid
  ];

  haskell-packages-shake = p: [
    p.base
    p.bytestring
    p.dhall_1_17_0
    p.directory
    p.shake
    p.typed-process
  ];

  haskell-packages-wai-middleware-rollbar = p: [
    p.aeson
    p.base
    p.bytestring
    p.hostname
    p.http-client
    p.http-conduit
    p.http-types
    p.rollbar-hs
    p.text
    p.time
    p.uuid
    p.wai
  ];

  nixpkgs = import ./nixpkgs.nix {};

  tools =
    tools-building ++
    tools-developing ++
    tools-formatting ++
    tools-static-analyzing;

  tools-building = [
    nixpkgs.bazel
    nixpkgs.haskellPackages.cabal-install
    nixpkgs.haskellPackages.hpack
  ];

  tools-developing = [
    nixpkgs.haskellPackages.ghcid
  ];

  tools-formatting = [
    nixpkgs.haskellPackages.stylish-haskell
    nixpkgs.nodePackages.prettier
  ];

  tools-static-analyzing = [
    nixpkgs.haskellPackages.hlint
    nixpkgs.pythonPackages.yamllint
  ];

in

  nixpkgs.stdenv.mkDerivation rec {
    inherit buildInputs;

    env = nixpkgs.buildEnv {
      inherit name;

      paths = buildInputs;
    };

    name = "open-source";
  }
