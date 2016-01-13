
# shell.nix references the project and adds developer tools,
# such that dependencies and tools are available when running
# nix-shell in this directory

{ env ? import ../env }:
let
  nixpkgs = env.nixpkgs;
  lib = nixpkgs.haskell-ng.lib;
  stdenv = nixpkgs.stdenv;
  packages = import ./packages.nix { inherit env; };
  haskell = nixpkgs.haskellngPackages.override { 
    overrides = self: super: 
      let p = packages.overrides self super;
      in {
      timekeeper = stdenv.lib.overrideDerivation (p.timekeeper.env) (old : {
      		     extraTools = [
                     self.cabal2nix
                   ];});
    };
  };
in haskell.timekeeper
