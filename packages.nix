
# define timekeeper using the fixed or supplied environment
# depends on timekeeper.nix generated by cabal2nix

{ env ? import ./env }:
let nixpkgs = env.nixpkgs;
    lib = nixpkgs.haskell-ng.lib;
in rec {
  overrides = self: super: {
      timekeeper = env.setSource ./timekeeper
                   (lib.addBuildTools
                     (self.callPackage timekeeper/timekeeper.nix {})
                     [ self.cabal2nix
		       self.ghc
                     ]
                   );
      nomadbase-toy = env.setSource ./nomadbase-toy
                   (lib.addBuildTools
                     (self.callPackage nomadbase-toy/nomadbase-toy.nix {})
                     [ self.cabal2nix
		       self.ghc
                     ]
                   );
      nomadbase-server = env.setSource ./nomadbase-server
                   (lib.addBuildTools
                     (self.callPackage nomadbase-server/nomadbase-server.nix {})
                     [ self.cabal2nix
		       self.ghc
                     ]
                   );
      nomadbase-algorithms = env.setSource ./nomadbase-algorithms
                   (lib.addBuildTools
                     (self.callPackage nomadbase-algorithms/nomadbase-algorithms.nix {})
                     [ self.cabal2nix
		       self.ghc
                     ]
                   );
    };
  packages = nixpkgs.haskellngPackages.override { 
    inherit overrides;
    };
  inherit (packages) nomadbase-toy nomadbase-server nomadbase-algorithms timekeeper;
}
