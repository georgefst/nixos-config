{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.05";
    nixpkgs-linux_6_16.url = "github:NixOS/nixpkgs/5a79545d3b917e23c1524763462fa6f9d084c5de";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    flake-utils.url = "github:numtide/flake-utils";
    agenix.url = "github:ryantm/agenix";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskell.follows = "haskellNix/nixpkgs-unstable";
    evdev-share.url = "github:georgefst/evdev-share";
    net-evdev.url = "github:georgefst/net-evdev";
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    obelisk = { url = "github:obsidiansystems/obelisk/develop"; flake = false; };
    hs-scripts.url = "github:georgefst/hs-scripts/nix";
    self.submodules = true;
  };
  outputs =
    inputs@{ self
    , nixpkgs
    , nixpkgs-unstable
    , nixos-hardware
    , flake-utils
    , agenix
    , ...
    }:
    let
      evalSystem = "x86_64-linux";
      buildSystem = evalSystem;
      buildPkgs = import nixpkgs { system = buildSystem; };

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
                  inherit evalSystem;
                  shell.tools = {
                    cabal = "latest";
                    haskell-language-server = "latest";
                  };
                };
            })
          ];
          inherit (inputs.haskellNix) config;
        }).hixProject.flake { });

      evdev-share = system: inputs.evdev-share.packages.${system}.default;
      net-evdev = system: nixpkgs.lib.getExe inputs.net-evdev.packages.${system}."net-evdev:exe:net-evdev";

      mandelbrot = { xMin, xMax, yMin, yMax }: buildPkgs.runCommand "mandelbrot" { } ''
        ${nixpkgs.lib.getExe inputs.hs-scripts.packages.${buildSystem}.mandelbrot} \
          --width 3840 --height 3840 \
          --xMin ${builtins.toString xMin} --xMax ${builtins.toString xMax} \
          --yMin ${builtins.toString yMin} --yMax ${builtins.toString yMax} \
          --out $out
      '';

      configs.sd = {
        clark =
          let
            system = "aarch64-linux";
          in
          nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              ./modules/universal.nix
              ./modules/users.nix
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
      };
      configs.desktop = {
        fry = hardwareModules:
          let
            system = "x86_64-linux";
          in
          nixpkgs.lib.nixosSystem {
            inherit system;
            modules = hardwareModules ++ [
              ./modules/universal.nix
              (import ./modules/desktop.nix
                {
                  hostName = "fry";
                  stateVersion = "25.05";
                  laptop = true;
                  wallpaper = mandelbrot { xMin = -3; xMax = 1.8; yMin = -2.4; yMax = 2.4; };
                }
                {
                  net-evdev = net-evdev system;
                }
              )
              ./modules/obsidian.nix
              {
                # 6.14 adds necessary support for our network card, but 6.12 is now the only maintained kernel with ZFS
                boot.kernelPackages = (import inputs.nixpkgs-linux_6_16 { inherit system; }).linuxPackages_6_16;
              }
              agenix.nixosModules.default
              ({ pkgs, ... }: {
                nixpkgs.overlays = nixpkgs.lib.mkBefore [
                  inputs.nix-vscode-extensions.overlays.default
                ];
                environment.systemPackages = [
                  (pkgs.callPackage inputs.obelisk { }).command
                  inputs.haskellNix.packages.${system}.hix
                ];
                system.nixos.tags = [ self.shortRev or self.dirtyShortRev ];
              })
              nixos-hardware.nixosModules.framework-amd-ai-300-series
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
        crow = hardwareModules:
          let
            system = "x86_64-linux";
          in
          nixpkgs-unstable.lib.nixosSystem {
            inherit system;
            modules = hardwareModules ++ [
              ./modules/universal.nix
              ./modules/users.nix
              (import ./modules/desktop.nix
                {
                  hostName = "crow";
                  stateVersion = "25.11";
                  wallpaper = mandelbrot { xMin = -1; xMax = -0.5; yMin = 0; yMax = 0.5; };
                  syncCamera = true;
                  keyboardLayout = "gb+mac";
                }
                {
                  net-evdev = net-evdev system;
                }
              )
              ./modules/apple-t2.nix
              {
                services.openssh.enable = true;
                systemd.services.magic-mouse = {
                  script = nixpkgs.lib.getExe haskell.packages.${system}."magic-mouse:exe:magic-mouse";
                  serviceConfig = { Restart = "always"; RestartSec = 1; };
                  unitConfig = { StartLimitIntervalSec = 0; };
                  description = "Magic mouse hack";
                  wantedBy = [ "multi-user.target" ];
                };
              }
              agenix.nixosModules.default
              ({ pkgs, ... }: {
                nixpkgs.overlays = nixpkgs.lib.mkBefore [
                  inputs.nix-vscode-extensions.overlays.default
                ];
                environment.systemPackages = [
                  (pkgs.callPackage inputs.obelisk { }).command
                  inputs.haskellNix.packages.${system}.hix
                ];
                system.nixos.tags = [ self.shortRev or self.dirtyShortRev ];
              })
              nixos-hardware.nixosModules.apple-t2
            ];
          };
      };

      nixosConfigurations = configs.sd // builtins.mapAttrs
        (name: system: system [ ./hardware-configuration/${name}.nix ])
        configs.desktop;

    in
    {
      inherit nixosConfigurations;
      images = builtins.mapAttrs (_: system: system.config.system.build.sdImage) configs.sd // builtins.mapAttrs
        (name: system: (system [
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-gnome.nix"
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
      inherit haskell;
    };
}
