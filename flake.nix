{
  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        builder = import ./builder.nix {
          inherit pkgs;
          ghc = pkgs.haskell.compiler.ghc8107;
          bootstrapFile = ./bootstrap/linux-8.10.7.json;
        };
      in {
        packages.default = builder;

        devShells.defaul = pkgs.mkShell {
          buildInputs = with pkgs; [ zlib ];
          name = "cabal-dev-shell";
        };
      });
}
