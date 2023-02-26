{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskell.follows = "haskellNix/nixpkgs-unstable";
    tennis-scraper.url = "/home/gthomas/code/tennis-scraper";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, nixpkgs-haskell, tennis-scraper }: rec {
    haskell = {
      clark = flake-utils.lib.eachSystem [ "aarch64-linux" ] (system:
        let
          overlays = [
            haskellNix.overlay
            (final: prev: {
              hixProject =
                final.haskell-nix.hix.project {
                  src = ./.;
                  compiler-nix-name = "ghc926";
                  index-state = "2023-02-25T00:00:00Z"; # TODO somehow we still get traces saying this isn't specified
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
    };
    nixosConfigurations = {
      clark = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          ./clark.nix
        ];
        specialArgs = {
          extraPkgs = {
            clark = haskell.clark.packages.aarch64-linux.default;
            tennis-scraper = tennis-scraper.packages.aarch64-linux.default;
          };
        };
      };
    };
    images.clark = nixosConfigurations.clark.config.system.build.sdImage;
  };
}
