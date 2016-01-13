rec {
  nixpkgs = import ../nixpkgs {};
  setSource = (dir: pkg: 
          nixpkgs.stdenv.lib.overrideDerivation pkg (oldAttrs: { src = filterDir dir; }));
  filterDir = builtins.filterSource (path: type: type != "unknown" 
		 && baseNameOf path != ".git"
                 && baseNameOf path != "dist"
                 && builtins.match "result.*" (baseNameOf path) == null);
}
