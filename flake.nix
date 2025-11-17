{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/ae814fd3904b621d8ab97418f1d0f2eb0d3716f4";
    nixpkgs-linux_6_16.url = "github:NixOS/nixpkgs/5a79545d3b917e23c1524763462fa6f9d084c5de";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    flake-utils.url = "github:numtide/flake-utils";
    agenix.url = "github:ryantm/agenix";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskell.follows = "haskell-nix/nixpkgs-unstable";
    evdev-share.url = "github:georgefst/evdev-share";
    net-evdev.url = "github:georgefst/net-evdev";
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    obelisk = { url = "github:obsidiansystems/obelisk/develop"; flake = false; };
    hs-scripts.url = "github:georgefst/hs-scripts/nix";
    self.submodules = true;
  };
  outputs = inputs@{ self, nixos-hardware, flake-utils, ... }:
    let
      evalSystem = "x86_64-linux";
      buildSystem = evalSystem;

      lib = inputs.nixpkgs.lib;
      packages = (flake-utils.lib.eachDefaultSystem (system: rec {
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
        packages = import inputs.nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
          overlays = [
            inputs.nix-vscode-extensions.overlays.default
            (final: prev: {
              clark = haskell.packages."clark:exe:clark";
              evdev-share = inputs.evdev-share.packages.${system}.default;
              hix = inputs.haskell-nix.packages.${system}.hix;
              magic-mouse = haskell.packages."magic-mouse:exe:magic-mouse";
              mandelbrot = inputs.hs-scripts.packages.${system}.mandelbrot;
              net-evdev = inputs.net-evdev.packages.${system}."net-evdev:exe:net-evdev";
              obelisk = (final.callPackage inputs.obelisk { }).command;
            })
          ];
        };
      })).packages;

      mandelbrot = { xMin, xMax, yMin, yMax }: packages.${buildSystem}.runCommand "mandelbrot" { } ''
        ${lib.getExe packages.${buildSystem}.mandelbrot} \
          --width 3840 --height 3840 \
          --xMin ${builtins.toString xMin} --xMax ${builtins.toString xMax} \
          --yMin ${builtins.toString yMin} --yMax ${builtins.toString yMax} \
          --out $out
      '';

      configs.sd.clark = lib.nixosSystem rec {
        system = "aarch64-linux";
        pkgs = packages.${system};
        modules = [
          (import ./modules/universal.nix { flake = self; })
          ./modules/users.nix
          "${inputs.nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          ./machines/clark.nix
          inputs.agenix.nixosModules.default
        ];
      };
      configs.desktop.fry = hardwareModules: lib.nixosSystem rec {
        system = "x86_64-linux";
        pkgs = packages.${system};
        modules = hardwareModules ++ [
          (import ./modules/universal.nix { flake = self; })
          (import ./modules/desktop.nix {
            hostName = "fry";
            stateVersion = "25.05";
            laptop = true;
            wallpaper = mandelbrot { xMin = -3; xMax = 1.8; yMin = -2.4; yMax = 2.4; };
          }
          )
          ./modules/obsidian.nix
          {
            # 6.14 adds necessary support for our network card, but 6.12 is now the only maintained kernel with ZFS
            boot.kernelPackages = (import inputs.nixpkgs-linux_6_16 { inherit system; }).linuxPackages_6_16;
          }
          inputs.agenix.nixosModules.default
          nixos-hardware.nixosModules.framework-amd-ai-300-series
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
      };
      configs.desktop.crow = hardwareModules: inputs.nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        pkgs = packages.${system};
        modules = hardwareModules ++ [
          (import ./modules/universal.nix { flake = self; })
          ./modules/users.nix
          (import ./modules/desktop.nix {
            hostName = "crow";
            stateVersion = "25.11";
            wallpaper = mandelbrot { xMin = -1; xMax = -0.5; yMin = 0; yMax = 0.5; };
            syncCamera = true;
            keyboardLayout = "gb+mac";
          }
          )
          ./modules/apple-t2.nix
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
          inputs.agenix.nixosModules.default
          nixos-hardware.nixosModules.apple-t2
        ];
      };

      nixosConfigurations = configs.sd // builtins.mapAttrs
        (name: system: system [ ./hardware-configuration/${name}.nix ])
        configs.desktop;

    in
    {
      inherit nixosConfigurations;
      images = builtins.mapAttrs (_: system: system.config.system.build.sdImage) configs.sd // builtins.mapAttrs
        (name: system: (system [
          "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-gnome.nix"
          ({ pkgs, ... }: {
            environment.systemPackages = [
              (pkgs.writeShellScriptBin "install-system" ''
                sudo nixos-install --system ${nixosConfigurations.${name}.config.system.build.toplevel} "$@"
              '')
            ];
          })
        ]).config.system.build.isoImage)
        configs.desktop;
      configs = builtins.mapAttrs (_: system: system.config.system.build.toplevel) nixosConfigurations;
      vms = builtins.mapAttrs (_: system: system.config.system.build.vm) nixosConfigurations;
      inherit packages;
    };
}
