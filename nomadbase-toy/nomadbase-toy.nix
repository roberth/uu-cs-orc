{ mkDerivation, base, stdenv, timekeeper }:
mkDerivation {
  pname = "nomadbase-toy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base timekeeper ];
  description = "Toy NomadBase for a single user";
  license = stdenv.lib.licenses.mit;
}
