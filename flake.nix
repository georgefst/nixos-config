{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
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
        in
        {
          inherit (haskell) devShells;
          packages = import inputs.nixpkgs {
            inherit system;
            config = nixpkgs-config;
            overlays = [
              inputs.nix-vscode-extensions.overlays.default
              (
                let pkgs-unstable = import inputs.nixpkgs-unstable { inherit system; config = nixpkgs-config; };
                in final: prev: {
                  # frequent updates are desirable
                  nixd = pkgs-unstable.nixd;
                  opencode = pkgs-unstable.opencode;
                  spotify = pkgs-unstable.spotify;
                  vscode = pkgs-unstable.vscode;
                  zed-editor = pkgs-unstable.zed-editor;
                  # non-Nixpkgs flake inputs
                  # TODO we should make sure this errors if we try to shadow something in nixpkgs
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
                }
              )
              # Opus 4.5 attempt to reconcile with `mouse-follows-focus`
              # idea is that we don't want the cursor to become visible when we switch focus via the extension
              # anyway, this "fix" has some weird behaviour, and crucially doesn't even fulfil its purpose
              # (import ./fixes/hide-cursor.nix)
              # also, reducing to minimum timeout (`timeout = 1`) potentially makes this not an issue in practice
              # time will tell
              # note that unlike when I first installed this extension, the timeout does actually work reliably, due to a significant rewrite for GNOME 49
              # EDIT while squashing everything: not sure that's true?
              (import ./fixes/opencode.nix)
              (import ./fixes/tiling-shell.nix)
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
      mkSdAndVm = arch: modules:
        let
          mkSystem = pkgs: hardwareModules: lib.nixosSystem { inherit pkgs; modules = modules ++ hardwareModules; };
        in
        {
          system = mkSystem packages.${arch}
            [ "${inputs.nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix" ];
          vm = mkSystem packages.${buildSystem}
            [ ];
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

      configs.sd.clark = mkSdAndVm "aarch64-linux"
        [
          (import ./modules/universal.nix { flake = self; syncCamera = true; })
          ./modules/users.nix
          ./modules/clark.nix
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
      inherit packages;

      images = builtins.mapAttrs (_: { system, ... }: system.config.system.build.sdImage) configs.sd //
        builtins.mapAttrs (_: { installer, ... }: installer.config.system.build.isoImage) configs.desktop;
      configs = builtins.mapAttrs (_: system: system.config.system.build.toplevel) nixosConfigurations;
      vms = builtins.mapAttrs (_: system: system.config.system.build.vm) nixosConfigurations //
        builtins.mapAttrs (_: { vm, ... }: vm.config.system.build.vm) configs.sd;
    };
}
