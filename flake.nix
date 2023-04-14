{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskell.follows = "haskellNix/nixpkgs-unstable";
    tennis-scraper = { url = "git+ssh://git@github.com/georgefst/tennis-scraper"; flake = false; };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }: rec {
    haskell =
      builtins.mapAttrs
        (_: { src, default }: flake-utils.lib.eachSystem [ "aarch64-linux" ] (system:
          let
            overlays = [
              inputs.haskellNix.overlay
              (final: prev: {
                hixProject =
                  final.haskell-nix.hix.project {
                    inherit src;
                    compiler-nix-name = "ghc927";
                    index-state = "2023-03-19T00:00:00Z";
                    evalSystem = "x86_64-linux";
                  };
              })
            ];
            pkgs = import inputs.nixpkgs-haskell { inherit system overlays; inherit (inputs.haskellNix) config; };
            flake = pkgs.hixProject.flake { };
          in
          flake // {
            packages.default = flake.packages."${default}";
          })
        )
        {
          clark = { src = ./.; default = "clark:exe:clark"; };
          tennis-scraper = { src = inputs.tennis-scraper; default = "tennis-scraper:exe:tennis-scraper"; };
        };

    nixosConfigurations = {
      clark = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          ./clark/clark.nix
        ];
        specialArgs = {
          extraPkgs = {
            clark = haskell.clark.packages.aarch64-linux.default;
            tennis-scraper = haskell.tennis-scraper.packages.aarch64-linux.default;
          };
        };
      };
    };

    images = builtins.mapAttrs (_: system: system.config.system.build.sdImage) nixosConfigurations;
    configs = builtins.mapAttrs (_: system: system.config.system.build.toplevel) nixosConfigurations;
    packages.x86_64-linux.default = configs.clark;
  };
}
