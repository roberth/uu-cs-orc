let env = import ./env;
    packages = import ./packages.nix { inherit env; };
    inherit (env) nixpkgs;
in nixpkgs.stdenv.mkDerivation {
    name = "nomadbase-aggregate";
    builder = ./aggregate.sh;
    buildInputs = [ nixpkgs.stdenv ];
    nomadbase_toy = packages.nomadbase-toy;
    nomadbase_server = packages.nomadbase-server;
    #proposal-presentation = import ./doc/proposal { inherit env; };
    inherit (packages) timekeeper;
    inherit (nixpkgs) coreutils;
}
