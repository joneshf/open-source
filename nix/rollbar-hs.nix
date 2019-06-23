{ mkDerivation, aeson, base, bytestring, case-insensitive, hostname
, hpack, hspec, hspec-golden-aeson, http-client, http-conduit
, http-types, network, QuickCheck, stdenv, text, time
, unordered-containers, uuid
}:
mkDerivation {
  pname = "rollbar-hs";
  version = "0.3.1.0";
  src = ../packages/rollbar-hs;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive hostname http-client
    http-conduit http-types network text time unordered-containers uuid
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive hspec hspec-golden-aeson
    QuickCheck text unordered-containers
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/joneshf/rollbar-hs#readme";
  description = "Core Rollbar data types and APIs";
  license = stdenv.lib.licenses.bsd3;
}
