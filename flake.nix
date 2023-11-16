{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    agenix.url = "github:ryantm/agenix";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskell.follows = "haskellNix/nixpkgs-unstable";
    evdev-share.url = "github:georgefst/evdev-share";
    tennis-scraper = { url = "git+ssh://git@github.com/georgefst/tennis-scraper"; flake = false; };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, agenix, ... }: rec {
    haskell =
      builtins.mapAttrs
        (name: pkg: (flake-utils.lib.eachDefaultSystem (system:
          let
            flake = (import inputs.nixpkgs-haskell {
              inherit system;
              overlays = [
                inputs.haskellNix.overlay
                (final: prev: {
                  hixProject =
                    final.haskell-nix.hix.project {
                      inherit (pkg) src
                        compiler-nix-name
                        index-state;
                      evalSystem = "x86_64-linux";
                    };
                })
              ];
              inherit (inputs.haskellNix) config;
            }).hixProject.flake { };
            default = "${name}:exe:${name}"; # only factored out because of issues with vscode syntax highlighter
          in
          flake // {
            packages = flake.packages // { default = flake.packages."${default}"; };
          }
        )))
        {
          clark = {
            src = ./.;
            compiler-nix-name = "ghc963";
            index-state = "2023-11-15T00:00:00Z";
          };
          tennis-scraper = {
            src = inputs.tennis-scraper;
            compiler-nix-name = "ghc928";
            index-state = "2023-06-28T00:00:00Z";
          };
        };

    nixosConfigurations = {
      clark = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          ./clark/clark.nix
          agenix.nixosModules.default
        ];
        specialArgs = {
          extraPkgs = builtins.listToAttrs
            (map (name: { inherit name; value = (inputs // haskell)."${name}".packages.aarch64-linux.default; })
              [ "clark" "evdev-share" "tennis-scraper" ]
            );
        };
      };
    };

    images = builtins.mapAttrs (_: system: system.config.system.build.sdImage) nixosConfigurations;
    configs = builtins.mapAttrs (_: system: system.config.system.build.toplevel) nixosConfigurations;

    # This is convenient while we only actually have one system, but will need changing eventually.
    packages.x86_64-linux.default = configs.clark;
  };
}
