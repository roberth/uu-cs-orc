{ mkDerivation, base, containers, free, HTF, HUnit, network
, QuickCheck, stdenv, stm, test-framework-hunit
, test-framework-quickcheck2, text
}:
mkDerivation {
  pname = "timekeeper";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers free network stm text ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers free HTF HUnit QuickCheck test-framework-hunit
    test-framework-quickcheck2 text
  ];
  description = "Distributed datastore for cluster metadata";
  license = stdenv.lib.licenses.mit;
}
