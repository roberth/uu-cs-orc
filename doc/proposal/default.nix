{ env ? import ../../env,
  nixpkgs ? env.nixpkgs,
  stdenv ? nixpkgs.stdenv }:
stdenv.mkDerivation {
  name = "proposal-presentation";
  version = "1";
  src = env.filterDir ./.;
  texlive = nixpkgs.texlive.combine {
    inherit (nixpkgs.texlive) scheme-small stmaryrd polytable lazylist;
  };
  lhs2tex = nixpkgs.haskellngPackages.lhs2tex;
}
