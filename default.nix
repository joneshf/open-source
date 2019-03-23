# The basic setup for the repo.
# We can have these tools everywhere in the repo,
# while also keeping track of them for the future.

let

  buildInputs =
    [ haskell-packages ] ++
    tools;

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          dhall_1_21_0 = super.callPackage ./nix/dhall_1_21_0.nix {
            megaparsec = self.megaparsec_7_0_4;

            repline = self.repline_0_2_0_0;
          };

          extra_1_6_4 = super.callPackage ./nix/extra_1_6_14.nix {};

          filepattern_0_1_1 = super.callPackage ./nix/filepattern_0_1_1.nix {
            extra = self.extra_1_6_4;
          };

          heaps_0_3_6_1 = super.callPackage ./nix/heaps_0_3_6_1.nix {};

          js-dgtable_0_5_2 = super.callPackage ./nix/js-dgtable_0_5_2.nix {};

          megaparsec_7_0_4 = super.callPackage ./nix/megaparsec_7_0_4.nix {};

          repline_0_2_0_0 = super.callPackage ./nix/repline_0_2_0_0.nix {};

          shake_0_17_7 = super.callPackage ./nix/shake_0_17_7.nix {
            extra = self.extra_1_6_4;

            filepattern = self.filepattern_0_1_1;

            heaps = self.heaps_0_3_6_1;

            js-dgtable = self.js-dgtable_0_5_2;
          };
        };
      };
    };
  };

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
    p.dhall_1_21_0
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
    p.dhall_1_21_0
    p.directory
    p.shake_0_17_7
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

  nixpkgs = import nixpkgs-tarball {
    inherit config;
  };

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
    nixpkgs.yarn
  ];

  # These are tools used when doing development.
  # These are useful when doing something in a cycle (like on a local computer).
  tools-developing = [
    nixpkgs.haskellPackages.cabal2nix
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
    nixpkgs.nodePackages.eslint
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
