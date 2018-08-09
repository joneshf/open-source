{ mkDerivation, aeson, base, bytestring, case-insensitive, hostname
, hspec, hspec-golden-aeson, http-client, http-conduit, http-types
, network, QuickCheck, stdenv, text, time, unordered-containers
, uuid
}:
mkDerivation {
  pname = "rollbar-hs";
  version = "0.3.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive hostname http-client
    http-conduit http-types network text time unordered-containers uuid
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive hspec hspec-golden-aeson
    QuickCheck text unordered-containers
  ];
  homepage = "https://github.com/joneshf/rollbar-hs#readme";
  description = "Core Rollbar data types and APIs";
  license = stdenv.lib.licenses.bsd3;
}
