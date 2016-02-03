
# define nomadbase-algorithms using the fixed or supplied environment
# depends on nomadbase-algorithms.nix generated by cabal2nix

{ env ? import ../env }:
let
  nixpkgs = env.nixpkgs;
  lib = nixpkgs.haskell-ng.lib;
  haskell = import ../packages.nix { inherit env; };
in haskell.nomadbase-algorithms