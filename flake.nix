{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    flake-utils.url = "github:numtide/flake-utils";
    agenix.url = "github:ryantm/agenix";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskell.follows = "haskellNix/nixpkgs-unstable";
    evdev-share.url = "github:georgefst/evdev-share";
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    obelisk = { url = "github:obsidiansystems/obelisk/develop"; flake = false; };
    hs-scripts.url = "github:georgefst/hs-scripts/nix";
    self.submodules = true;
  };
  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, nixos-hardware, flake-utils, agenix, ... }:
    let
      haskell = flake-utils.lib.eachDefaultSystem (system:
            (import inputs.nixpkgs-haskell {
              inherit system;
              overlays = [
                inputs.haskellNix.overlay
                (final: prev: {
                  hixProject =
                    final.haskell-nix.hix.project {
                      src = ./.;
                      compiler-nix-name = "ghc9122";
                      index-state = "2025-09-02T00:00:00Z";
                      evalSystem = "x86_64-linux";
                    };
                })
              ];
              inherit (inputs.haskellNix) config;
            }).hixProject.flake { });

      evdev-share = system: inputs.evdev-share.packages.${system}.default;

        mandelbrot = system: pkgs: { xMin, xMax, yMin, yMax }: pkgs.runCommand "mandelbrot" { } ''
          ${pkgs.lib.getExe inputs.hs-scripts.packages.${system}.mandelbrot} \
            --width 3840 --height 3840 \
            --xMin ${builtins.toString xMin} --xMax ${builtins.toString xMax} \
            --yMin ${builtins.toString yMin} --yMax ${builtins.toString yMax} \
            --out $out
        '';

      nixosConfigurations = {
        clark =
          let
            system = "aarch64-linux";
          in
          nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              ./util/common.nix
              ./util/common-users.nix
              "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
              ./machines/clark.nix
              agenix.nixosModules.default
            ];
            specialArgs = {
              extraPkgs = {
                clark = haskell.packages.${system}."clark:exe:clark";
                evdev-share = evdev-share system;
              };
            };
          };
        fry =
          let
            system = "x86_64-linux";
          in
          nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              ./util/common.nix
              (args@{ pkgs, ... }: (import ./util/common-desktop.nix
                {
                  hostName = "fry";
                  stateVersion = "25.05";
                  wallpaper = mandelbrot system pkgs { xMin = -3; xMax = 1.8; yMin = -2.4; yMax = 2.4; };
                }
                args)
              )
              ./hardware-configuration/fry.nix
              ./machines/fry.nix
              ./obsidian
              ./obsidian/users
              agenix.nixosModules.default
              { nixpkgs.overlays = nixpkgs.lib.mkBefore [ inputs.nix-vscode-extensions.overlays.default ]; }
              ({ pkgs, ... }: { environment.systemPackages = [ (pkgs.callPackage inputs.obelisk { }).command ]; })
              { environment.systemPackages = [ inputs.haskellNix.packages.${system}.hix ]; }
              { system.nixos.tags = [ self.shortRev or self.dirtyShortRev ]; }
              {
                # avoid some broken caches
                options.nix.settings.substituters = nixpkgs.lib.mkOption {
                  apply = nixpkgs.lib.filter (s: !(
                    s == "s3://obsidian-open-source" ||
                    nixpkgs.lib.hasPrefix "http://obsidian.webhop.org" s
                  ));
                };
              }
            ];
          };
        crow =
          let
            system = "x86_64-linux";
          in
          nixpkgs-unstable.lib.nixosSystem {
            inherit system;
            modules = [
              ./util/common.nix
              ./util/common-users.nix
              (args@{ pkgs, ... }: (import ./util/common-desktop.nix
                {
                  hostName = "crow";
                  stateVersion = "25.11";
                  wallpaper = mandelbrot system pkgs { xMin = -1; xMax = -0.5; yMin = 0; yMax = 0.5; };
                  syncCamera = true;
                  keyboardLayout = "gb+mac";
                }
                args)
              )
              ./hardware-configuration/crow.nix
              ./machines/crow.nix
              agenix.nixosModules.default
              { nixpkgs.overlays = nixpkgs.lib.mkBefore [ inputs.nix-vscode-extensions.overlays.default ]; }
              { environment.systemPackages = [ inputs.haskellNix.packages.${system}.hix ]; }
              { system.nixos.tags = [ self.shortRev or self.dirtyShortRev ]; }
              nixos-hardware.nixosModules.apple-t2
            ];
            specialArgs.magic-mouse = inputs.hs-scripts.packages.${system}.magicMouse;
          };
      };

    in
    haskell // {
      inherit nixosConfigurations;
      images = builtins.mapAttrs (_: system: system.config.system.build.sdImage) nixosConfigurations;
      configs = builtins.mapAttrs (_: system: system.config.system.build.toplevel) nixosConfigurations;
      vms = builtins.mapAttrs (_: system: system.config.system.build.vm) nixosConfigurations;
    };
}
