{
  inputs = {
    utils.url = "github:numtide/flake-utils";
    easy-purescript.url = "github:justinwoo/easy-purescript-nix";
  };
  outputs =
    {
      self,
      nixpkgs,
      easy-purescript,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        easy-ps = easy-purescript.packages.${system};
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.nodejs-18_x
            pkgs.esbuild
            easy-ps.purs-0_15_15
            easy-ps.spago
          ];
        };
      }
    );
}
