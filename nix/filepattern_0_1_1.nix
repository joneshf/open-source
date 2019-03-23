{ mkDerivation, base, directory, extra, filepath, QuickCheck
, stdenv
}:
mkDerivation {
  pname = "filepattern";
  version = "0.1.1";
  sha256 = "f7fc5bdcfef0d43a793a3c64e7c0fd3b1d35eea97a37f0e69d6612ab255c9b4b";
  libraryHaskellDepends = [ base directory extra filepath ];
  testHaskellDepends = [ base directory extra filepath QuickCheck ];
  homepage = "https://github.com/ndmitchell/filepattern#readme";
  description = "File path glob-like matching";
  license = stdenv.lib.licenses.bsd3;
}
