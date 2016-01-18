{ mkDerivation, base, stdenv, timekeeper }:
mkDerivation {
  pname = "nomadbase-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base timekeeper ];
  description = "NomadBase server";
  license = stdenv.lib.licenses.mit;
}
