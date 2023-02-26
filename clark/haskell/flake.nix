{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs-haskell.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs-haskell, flake-utils, haskellNix }:
    let
      supportedSystems = [ "aarch64-linux" ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                compiler-nix-name = "ghc926";
                evalSystem = "x86_64-linux";
              };
          })
        ];
        pkgs = import nixpkgs-haskell { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake { };
      in
      flake // {
        legacyPackages = pkgs;
        packages.default = flake.packages."clark:exe:clark";
      });
}
