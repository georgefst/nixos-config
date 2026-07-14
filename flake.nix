{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "flake-utils/systems";
    };
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskell.follows = "haskell-nix/nixpkgs-unstable";
    evdev-share.url = "github:georgefst/evdev-share";
    net-evdev = {
      url = "github:georgefst/net-evdev";
      inputs.flake-utils.follows = "flake-utils";
      inputs.haskellNix.follows = "haskell-nix";
    };
    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hs-scripts = {
      url = "github:georgefst/hs-scripts/nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lifx-manager = {
      url = "github:georgefst/lifx-manager/nix"; # https://github.com/georgefst/lifx-manager/pull/17
      inputs.flake-utils.follows = "flake-utils";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs-haskell";
    };
    self.submodules = true;
  };
  outputs = inputs@{ self, nixos-hardware, flake-utils, ... }:
    let
      evalSystem = "x86_64-linux";
      buildSystem = evalSystem;

      lib = inputs.nixpkgs.lib;
      inherit (flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
        let
          nixpkgs-config = {
            allowUnfree = true;
          };
          haskell = (import inputs.nixpkgs-haskell {
            inherit system;
            overlays = [
              inputs.haskell-nix.overlay
              (final: prev: {
                hixProject =
                  final.haskell-nix.hix.project {
                    src = ./.;
                    compiler-nix-name = "ghc912";
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
          extraPackages =
            let pkgs-unstable = import inputs.nixpkgs-unstable { inherit system; config = nixpkgs-config; };
            in {
              # frequent updates are desirable
              nixd = pkgs-unstable.nixd;
              opencode = pkgs-unstable.opencode;
              spotify = pkgs-unstable.spotify;
              vscode = pkgs-unstable.vscode;
              zed-editor = pkgs-unstable.zed-editor;
              inherit (pkgs-unstable) kdePackages; # Bigscreen requires KDE 6.7, which is only in unstable
              # non-Nixpkgs flake inputs
              agenix = inputs.agenix.packages.${system}.default;
              evdev-share = inputs.evdev-share.packages.${system}.default;
              hix = inputs.haskell-nix.packages.${system}.hix;
              mandelbrot = inputs.hs-scripts.packages.${system}.mandelbrot;
              lifx-manager = inputs.lifx-manager.packages.${system}.lifx-manager;
              net-evdev = inputs.net-evdev.packages.${system}."net-evdev:exe:net-evdev";
              # developed locally
              clark = haskell.packages."clark:exe:clark";
              magic-mouse = haskell.packages."magic-mouse:exe:magic-mouse";
              qr = haskell.packages."qr:exe:qr";
              sol = haskell.packages."sol:exe:sol";
            };
        in
        {
          inherit (haskell) devShells;
          inherit extraPackages;
          packages = import inputs.nixpkgs {
            inherit system;
            config = nixpkgs-config;
            overlays = [
              inputs.nix-vscode-extensions.overlays.default
              (_: _: extraPackages)
              (import ./fixes/opencode.nix)
              (import ./fixes/tiling-shell.nix)
            ];
          };
        })) packages extraPackages devShells;

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
      mkPiSdAndVm = hardwareModules: modules:
        let overlays = system: [ (_: _: extraPackages.${system}) ]; in {
          system = inputs.nixos-raspberrypi.lib.nixosSystem {
            modules = modules ++ hardwareModules ++ [
              inputs.nixos-raspberrypi.nixosModules.sd-image
              { nixpkgs.overlays = overlays "aarch64-linux"; }
            ];
          };
          vm = lib.nixosSystem {
            pkgs = packages.${buildSystem};
            modules = modules ++ [
              { nixpkgs.overlays = overlays buildSystem; }
            ];
          };
        };

      mandelbrot = let pkgs = packages.${buildSystem}; in { x, y, size, inverted ? false }: pkgs.runCommand "mandelbrot"
        { nativeBuildInputs = [ pkgs.imagemagick ]; } ''
        ${lib.getExe pkgs.mandelbrot} \
          --width 3840 --height 3840 \
          --centreX ${toString x} --centreY ${toString y} \
          --size ${toString size} \
          --innerColour "hsl(213, 76%, ${if inverted then "2%" else "55%"})" \
          --outerColour "hsl(213, 76%, ${if inverted then "55%" else "2%"})" \
          --out raw.png
        magick raw.png -dither FloydSteinberg PNG8:$out
      '';

      configs.sd.clark = mkPiSdAndVm
        [
          inputs.nixos-raspberrypi.nixosModules.raspberry-pi-3.base
          {
            hardware.raspberry-pi.config.all = {
              dt-overlays.hifiberry-dacplus.enable = true;
              base-dt-params.audio.enable = lib.mkForce false;
              dt-overlays.vc4-kms-v3d.params.noaudio.enable = true;
            };
          }
        ]
        [
          (import ./modules/universal.nix { flake = self; syncCamera = true; })
          ./modules/users.nix
          ./modules/clark.nix
          inputs.agenix.nixosModules.default
        ];
      configs.sd.sol = mkPiSdAndVm
        [
          inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5.base
          inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5.bluetooth
          inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5.display-vc4
          {
            hardware.raspberry-pi.config.all = {
              dt-overlays.hifiberry-dacplusdsp.enable = true;
              base-dt-params.audio.enable = lib.mkForce false;
              dt-overlays.vc4-kms-v3d.params.noaudio.enable = true;
            };
          }
        ]
        [
          (import ./modules/universal.nix { flake = self; })
          ./modules/users.nix
          ./modules/sol.nix
          inputs.agenix.nixosModules.default
        ];
      configs.desktop.fry = mkDesktopAndInstaller "fry" (hostName: hardwareModules: lib.nixosSystem {
        pkgs = packages.x86_64-linux;
        modules = hardwareModules ++ [
          (import ./modules/universal.nix { flake = self; })
          (import ./modules/desktop.nix {
            inherit hostName;
            stateVersion = "25.05";
            laptop = true;
            wallpaper = mandelbrot { x = -0.6; y = 0; size = 4.8; };
          })
          ./modules/obsidian.nix
          ./modules/airpods-hfp-fix.nix
          nixos-hardware.nixosModules.framework-amd-ai-300-series
          inputs.agenix.nixosModules.default
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
          (import ./modules/universal.nix { flake = self; syncCamera = true; })
          ./modules/users.nix
          (import ./modules/desktop.nix {
            inherit hostName;
            stateVersion = "25.11";
            wallpaper = mandelbrot { x = -0.8; y = -0.2; size = 0.5; inverted = true; };
            keyboardLayout = "gb+mac";
          })
          ./modules/apple-t2.nix
          nixos-hardware.nixosModules.apple-t2
          inputs.agenix.nixosModules.default
          ({ pkgs, ... }: {
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

      nixosConfigurations = builtins.mapAttrs (_: { system, ... }: system)
        (configs.sd // configs.desktop);

    in
    {
      inherit devShells;
      inherit nixosConfigurations;
      packages = extraPackages;

      images = builtins.mapAttrs (_: { system, ... }: system.config.system.build.sdImage) configs.sd //
        builtins.mapAttrs (_: { installer, ... }: installer.config.system.build.isoImage) configs.desktop;
      configs = builtins.mapAttrs (_: system: system.config.system.build.toplevel) nixosConfigurations;
      vms = builtins.mapAttrs (_: system: system.config.system.build.vm) nixosConfigurations //
        builtins.mapAttrs (_: { vm, ... }: vm.config.system.build.vm) configs.sd;
    };
}
