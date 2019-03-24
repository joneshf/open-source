{ mkDerivation, base, clock, directory, filepath, process
, QuickCheck, stdenv, time, unix
}:
mkDerivation {
  pname = "extra";
  version = "1.6.14";
  sha256 = "a60641530d96653ecc365aa042f4061892154995915d91f432ea5a2e3aaf129c";
  libraryHaskellDepends = [
    base clock directory filepath process time unix
  ];
  testHaskellDepends = [ base directory filepath QuickCheck unix ];
  homepage = "https://github.com/ndmitchell/extra#readme";
  description = "Extra functions I use";
  license = stdenv.lib.licenses.bsd3;
}
