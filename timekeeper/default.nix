{ env ? import ../env }:
let
  nixpkgs = env.nixpkgs;
  lib = nixpkgs.haskell-ng.lib;
  stdenv = nixpkgs.stdenv;
  haskell = nixpkgs.haskellngPackages.override { 
    overrides = self: super: {
      timekeeper = haskell.mkDerivation {
        pname = "timekeeper";
        version = "0.1.0.0";
        src = env.filterDir ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = (with self; [ 
	    		 base
 			 async
			 monad-par
			 time
			 free
			 xml
			 utf8-string
			 HTTP
			 network
			 distributed-process
			 distributed-process-supervisor
			 distributed-process-simplelocalnet
			 distributed-process-registry
			 distributed-process-p2p
			 distributed-process-async
			 test-framework
			 test-framework-quickcheck2
			 test-framework-hunit
			 QuickCheck
			 HUnit
			 HTF
			 ]);
        buildTools = [ ];
        license = stdenv.lib.licenses.mit;
      };
    };
  };
in haskell.timekeeper
