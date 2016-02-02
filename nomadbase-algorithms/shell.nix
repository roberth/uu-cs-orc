
# shell.nix references the project and adds developer tools,
# such that dependencies and tools are available when running
# nix-shell in this directory

{ env ? import ../env }:
let
  nixpkgs = env.nixpkgs;
  lib = nixpkgs.haskell-ng.lib;
  packages = import ../packages.nix { inherit env; };
  haskell = packages.packages;
in nixpkgs.stdenv.lib.overrideDerivation packages.nomadbase-algorithms.env (old: {
                     nativeBuildInputs = old.nativeBuildInputs ++ [
                                       haskell.cabal2nix
                                       haskell.cabal-install
                                       nixpkgs.inotify-tools
                                       ];
                     shellHook = ''
                       if test -f ~/.x-nix-shellrc; then . ~/.x-nix-shellrc; fi
                     '';
  })
