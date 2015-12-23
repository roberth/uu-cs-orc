{
  nixpkgs = import ../nixpkgs {};
  filterDir = builtins.filterSource (path: type: type != "unknown" 
		 && baseNameOf path != ".git"
                 && baseNameOf path != "dist"
                 && builtins.match "result.*" (baseNameOf path) == null);
}
