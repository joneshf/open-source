let

  revision = "d3d465e32f53dd88c2b4229c207ff7d005098f1d";

  sha256 = "sha256:092zf82gh6hn6y6zksxqv70ncpcimd51c3mj4mbrxpwl3w0gg3qd";

  tarball = builtins.fetchTarball {
    inherit sha256 url;
  };

  url = "https://github.com/NixOS/nixpkgs/archive/${revision}.tar.gz";

in

  import tarball
