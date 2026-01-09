{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    flake-utils.url = "github:numtide/flake-utils";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "flake-utils/systems";
    };
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskell.follows = "haskell-nix/nixpkgs-unstable";
    evdev-share = {
      url = "github:georgefst/evdev-share";
      inputs.cargo2nix.inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "haskell-nix/flake-compat";
        flake-utils.follows = "flake-utils";
      };
    };
    net-evdev = {
      url = "github:georgefst/net-evdev";
      inputs.flake-utils.follows = "flake-utils";
      inputs.haskellNix.follows = "haskell-nix";
    };
    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    obelisk = { url = "github:obsidiansystems/obelisk/develop"; flake = false; };
    hs-scripts = {
      url = "github:georgefst/hs-scripts/nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    self.submodules = true;
  };
  outputs = inputs@{ self, nixos-hardware, flake-utils, ... }:
    let
      evalSystem = "x86_64-linux";
      buildSystem = evalSystem;

      lib = inputs.nixpkgs.lib;
      inherit (flake-utils.lib.eachDefaultSystem (system:
        let
          haskell = (import inputs.nixpkgs-haskell {
            inherit system;
            overlays = [
              inputs.haskell-nix.overlay
              (final: prev: {
                hixProject =
                  final.haskell-nix.hix.project {
                    src = ./.;
                    compiler-nix-name = "ghc9122";
                    index-state = "2025-09-02T00:00:00Z";
                    inherit evalSystem;
                    shell.tools = {
                      cabal = "latest";
                      haskell-language-server = "latest";
                    };
                  };
              })
            ];
            config = inputs.haskell-nix.config;
          }).hixProject.flake { };
        in
        {
          inherit (haskell) devShells;
          packages = import inputs.nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
            };
            overlays = [
              inputs.nix-vscode-extensions.overlays.default
              (
                let
                  pkgs-unstable = import inputs.nixpkgs-unstable {
                    inherit system;
                    config.allowUnfree = true;
                  };
                in
                (final: prev: {
                  opencode = pkgs-unstable.opencode;
                })
              )
              (final: prev: {
                clark = haskell.packages."clark:exe:clark";
                evdev-share = inputs.evdev-share.packages.${system}.default;
                hix = inputs.haskell-nix.packages.${system}.hix;
                magic-mouse = haskell.packages."magic-mouse:exe:magic-mouse";
                mandelbrot = inputs.hs-scripts.packages.${system}.mandelbrot;
                net-evdev = inputs.net-evdev.packages.${system}."net-evdev:exe:net-evdev";
                obelisk = (final.callPackage inputs.obelisk { inherit system; }).command;
              })
            ];
          };
        })) packages devShells;

      mkDesktopAndInstaller = name: mkSystem: rec {
        system = mkSystem name [ ./hardware-configuration/${name}.nix ];
        installer = mkSystem name [
          "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-gnome.nix"
          {
            environment.systemPackages = [
              (packages.${buildSystem}.writeShellScriptBin "install-system" ''
                sudo nixos-install --system ${system.config.system.build.toplevel} "$@"
              '')
            ];
          }
        ];
      };

      mandelbrot = { xMin, xMax, yMin, yMax }: packages.${buildSystem}.runCommand "mandelbrot" { } ''
        ${lib.getExe packages.${buildSystem}.mandelbrot} \
          --width 3840 --height 3840 \
          --xMin ${builtins.toString xMin} --xMax ${builtins.toString xMax} \
          --yMin ${builtins.toString yMin} --yMax ${builtins.toString yMax} \
          --out $out
      '';

      configs.sd.clark = lib.nixosSystem {
        pkgs = packages.aarch64-linux;
        modules = [
          (import ./modules/universal.nix { flake = self; })
          ./modules/users.nix
          "${inputs.nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          ./machines/clark.nix
          inputs.agenix.nixosModules.default
        ];
      };
      configs.desktop.fry = mkDesktopAndInstaller "fry" (hostName: hardwareModules: lib.nixosSystem {
        pkgs = packages.x86_64-linux;
        modules = hardwareModules ++ [
          (import ./modules/universal.nix { flake = self; })
          (import ./modules/desktop.nix {
            inherit hostName;
            stateVersion = "25.05";
            laptop = true;
            wallpaper = mandelbrot { xMin = -3; xMax = 1.8; yMin = -2.4; yMax = 2.4; };
          })
          ./modules/obsidian.nix
          nixos-hardware.nixosModules.framework-amd-ai-300-series
          inputs.agenix.nixosModules.default
          ({ pkgs, ... }: {
            # ZFS 2.4 is needed for 6.18 kernel, but NixOS 25.11 uses ZFS 2.3 by default
            boot.zfs.package = pkgs.zfs_2_4;
          })
          {
            # avoid some broken caches
            options.nix.settings.substituters = lib.mkOption {
              apply = lib.filter (s: !(
                s == "s3://obsidian-open-source" ||
                  lib.hasPrefix "http://obsidian.webhop.org" s
              ));
            };
          }
        ];
      });
      configs.desktop.crow = mkDesktopAndInstaller "crow" (hostName: hardwareModules: lib.nixosSystem {
        pkgs = packages.x86_64-linux;
        modules = hardwareModules ++ [
          (import ./modules/universal.nix { flake = self; })
          ./modules/users.nix
          (import ./modules/desktop.nix {
            inherit hostName;
            stateVersion = "25.11";
            wallpaper = mandelbrot { xMin = -1; xMax = -0.5; yMin = 0; yMax = 0.5; };
            syncCamera = true;
            keyboardLayout = "gb+mac";
          })
          ./modules/apple-t2.nix
          nixos-hardware.nixosModules.apple-t2
          inputs.agenix.nixosModules.default
          ({ pkgs, ... }: {
            services.openssh.enable = true;
            systemd.services.magic-mouse = {
              script = lib.getExe pkgs.magic-mouse;
              serviceConfig = { Restart = "always"; RestartSec = 1; };
              unitConfig = { StartLimitIntervalSec = 0; };
              description = "Magic mouse hack";
              wantedBy = [ "multi-user.target" ];
            };
          })
        ];
      });

      nixosConfigurations = configs.sd
        // builtins.mapAttrs (_: { system, ... }: system) configs.desktop;

    in
    {
      inherit devShells;
      inherit nixosConfigurations;
      inherit packages;

      images = builtins.mapAttrs (_: system: system.config.system.build.sdImage) configs.sd //
        builtins.mapAttrs (_: { installer, ... }: installer.config.system.build.isoImage) configs.desktop;
      configs = builtins.mapAttrs (_: system: system.config.system.build.toplevel) nixosConfigurations;
      vms = builtins.mapAttrs (_: system: system.config.system.build.vm) nixosConfigurations;
    };
}
