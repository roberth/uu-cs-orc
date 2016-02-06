{ mkDerivation, base, containers, free, HTF, HUnit, QuickCheck
, stdenv, test-framework-hunit, test-framework-quickcheck2, text
, timekeeper
}:
mkDerivation {
  pname = "nomadbase-algorithms";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base text timekeeper ];
  testHaskellDepends = [
    base containers free HTF HUnit QuickCheck test-framework-hunit
    test-framework-quickcheck2 text timekeeper
  ];
  description = "Example algorithms on NomadBase";
  license = stdenv.lib.licenses.mit;
}
