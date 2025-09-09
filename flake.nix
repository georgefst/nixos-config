{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
    agenix.url = "github:ryantm/agenix";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskell.follows = "haskellNix/nixpkgs-unstable";
    evdev-share.url = "github:georgefst/evdev-share";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, agenix, ... }: rec {
    haskell =
      builtins.mapAttrs
        (name: src: (flake-utils.lib.eachDefaultSystem (system:
          let
            flake = (import inputs.nixpkgs-haskell {
              inherit system;
              overlays = [
                inputs.haskellNix.overlay
                (final: prev: {
                  hixProject =
                    final.haskell-nix.hix.project {
                      inherit src;
                      compiler-nix-name = "ghc9122";
                      index-state = "2025-09-02T00:00:00Z";
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
          clark = ./.;
        };

    nixosConfigurations = {
      clark =
        let
          system = "aarch64-linux";
        in
        nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
            ./machines/clark.nix
            agenix.nixosModules.default
          ];
          specialArgs = {
            extraPkgs = builtins.listToAttrs
              (map (name: { inherit name; value = (inputs // haskell)."${name}".packages."${system}".default; })
                [ "clark" "evdev-share" ]
              );
          };
        };
    };

    images = builtins.mapAttrs (_: system: system.config.system.build.sdImage) nixosConfigurations;
    configs = builtins.mapAttrs (_: system: system.config.system.build.toplevel) nixosConfigurations;
    vms = builtins.mapAttrs (_: system: system.config.system.build.vm) nixosConfigurations;

    # This is convenient while we only actually have one system, but will need changing eventually.
    packages.x86_64-linux.default = configs.clark;
  };
}
