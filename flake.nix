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
        (name: src: (flake-utils.lib.eachSystem [ "aarch64-linux" ] (system:
          (import inputs.nixpkgs-haskell {
            inherit system;
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
            inherit (inputs.haskellNix) config;
          }).hixProject.flake { }
        )).packages.aarch64-linux."${name}:exe:${name}")
        {
          clark = ./.;
          tennis-scraper = inputs.tennis-scraper;
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
            inherit (haskell)
              clark
              tennis-scraper;
          };
        };
      };
    };

    images = builtins.mapAttrs (_: system: system.config.system.build.sdImage) nixosConfigurations;
    configs = builtins.mapAttrs (_: system: system.config.system.build.toplevel) nixosConfigurations;

    # This is convenient while we only actually have one system, but will need changing eventually.
    packages.x86_64-linux.default = configs.clark;
  };
}
