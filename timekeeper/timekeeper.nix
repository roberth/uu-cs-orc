{ mkDerivation, base, binary, containers, distributed-process
, distributed-process-simplelocalnet, distributed-static, free, HTF
, HUnit, network, QuickCheck, stdenv, stm, template-haskell
, test-framework-hunit, test-framework-quickcheck2, text
}:
mkDerivation {
  pname = "timekeeper";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary containers distributed-process
    distributed-process-simplelocalnet distributed-static free network
    stm template-haskell text
  ];
  testHaskellDepends = [
    base containers free HTF HUnit QuickCheck test-framework-hunit
    test-framework-quickcheck2 text
  ];
  description = "Distributed datastore for cluster metadata";
  license = stdenv.lib.licenses.mit;
}
