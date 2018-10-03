# The basic setup for the repo.
# We can have these tools everywhere in the repo,
# while also keeping track of them for the future.

let

  buildInputs = [
  ];

  nixpkgs = import nixpkgs-tarball {};

  nixpkgs-tarball = builtins.fetchTarball {
    inherit sha256 url;
  };

  revision = "d3d465e32f53dd88c2b4229c207ff7d005098f1d";

  sha256 = "sha256:092zf82gh6hn6y6zksxqv70ncpcimd51c3mj4mbrxpwl3w0gg3qd";

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
