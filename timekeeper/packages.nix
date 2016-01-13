
# define timekeeper using the fixed or supplied environment
# depends on timekeeper.nix generated by cabal2nix

{ env ? import ../env }:
let nixpkgs = env.nixpkgs;
    lib = nixpkgs.haskell-ng.lib;
in rec {
  overrides = self: super: {
      timekeeper = env.setSource ./.
                   (lib.addBuildTools
                     (self.callPackage ./timekeeper.nix {})
                     [ self.cabal2nix
		       self.ghc
                     ]
                   );
    };
  packages = nixpkgs.haskellngPackages.override { 
    inherit overrides;
    };
  timekeeper = packages.timekeeper;

}