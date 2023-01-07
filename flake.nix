{
  inputs = {
    nixpkgs.url = "nixpkgs";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, utils, nixpkgs }: utils.lib.eachDefaultSystem (system:
  let
    name = "gorn";
    pkgs = import nixpkgs { inherit system; };
    haskellPackages = pkgs.haskellPackages;
  in
    {
      packages = rec {
        default = gorn;
        gorn = haskellPackages.callCabal2nix name ./. {};

        container = let
          version = self.packages.${system}.${name}.version;
        in pkgs.dockerTools.buildLayeredImage {
          inherit name;
          tag = "${version}-${self.sourceInfo.shortRev or "dirty"}";

          contents = [ default ];
          created = "now";
        };
      };

      devShell = haskellPackages.shellFor {
        packages = p: [ self.packages.${system}.gorn ];
        withHoogle = true;
        buildInputs = with haskellPackages; [
          haskell-language-server
          ghcid
          cabal-install
        ];
      };
  });
}
