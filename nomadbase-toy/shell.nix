
# shell.nix references the project and adds developer tools,
# such that dependencies and tools are available when running
# nix-shell in this directory

{ env ? import ../env }:
let
  nixpkgs = env.nixpkgs;
  lib = nixpkgs.haskell-ng.lib;
  stdenv = nixpkgs.stdenv;
  packages = import ../packages.nix { inherit env; };
  haskell = nixpkgs.haskellngPackages.override { 
    overrides = self: super:
      let p = packages.overrides self super;
      in {
      nomadbase-toy = stdenv.lib.overrideDerivation (p.nomadbase-toy.env) (old : {
                     nativeBuildInputs = old.nativeBuildInputs ++ [
                                       self.cabal2nix
                                       self.cabal-install
                                       nixpkgs.inotify-tools
                                       ];
                     shellHook = old.shellHook + ''
                       if test -f ~/.x-nix-shellrc; then . ~/.x-nix-shellrc; fi
                     '';
                   });
    };
  };
in haskell.nomadbase-toy
