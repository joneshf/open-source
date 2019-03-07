# The basic setup for the repo.
# We can have these tools everywhere in the repo,
# while also keeping track of them for the future.

let

  buildInputs =
    [ haskell-packages ] ++
    tools;

  haskell-packages = nixpkgs.haskellPackages.ghcWithHoogle (p:
    haskell-packages-dhall-javascript p ++
    haskell-packages-katip-rollbar p ++
    haskell-packages-rollbar-hs p ++
    haskell-packages-shake p ++
    haskell-packages-wai-middleware-rollbar p
  );

  haskell-packages-dhall-javascript = p: [
    p.ansi-wl-pprint
    p.base
    p.dhall_1_17_0
    p.freer-simple
    p.language-ecmascript
    p.optparse-applicative
    p.scientific
    p.text
  ];

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

  nixpkgs = import nixpkgs-tarball {};

  nixpkgs-tarball = builtins.fetchTarball {
    inherit sha256 url;
  };

  revision = "d3d465e32f53dd88c2b4229c207ff7d005098f1d";

  sha256 = "sha256:092zf82gh6hn6y6zksxqv70ncpcimd51c3mj4mbrxpwl3w0gg3qd";

  tools =
    tools-building ++
    tools-developing ++
    tools-formatting ++
    tools-static-analyzing;

  # These are tools necessary to build packages and programs.
  # These are for one-off builds (like in CI).
  tools-building = [
    nixpkgs.haskellPackages.cabal-install
    nixpkgs.haskellPackages.hpack
  ];

  # These are tools used when doing development.
  # These are useful when doing something in a cycle (like on a local computer).
  tools-developing = [
    nixpkgs.haskellPackages.ghcid
  ];

  # These are tools that format source code.
  tools-formatting = [
    nixpkgs.haskellPackages.stylish-haskell
    nixpkgs.nodePackages.prettier
  ];

  # These are tools that analyze source code in some way.
  tools-static-analyzing = [
    nixpkgs.haskellPackages.hlint
    nixpkgs.pythonPackages.yamllint
  ];

  url = "https://github.com/NixOS/nixpkgs/archive/${revision}.tar.gz";

in

  nixpkgs.stdenv.mkDerivation rec {
    inherit buildInputs;

    env = nixpkgs.buildEnv {
      inherit name;

      paths = buildInputs;
    };

    name = "open-source";
  }
