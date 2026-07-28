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
    hls = { url = "github:haskell/haskell-language-server"; flake = false; }; # https://github.com/haskell/haskell-language-server/pull/5009
    browser-wasi-shim = { url = "https://registry.npmjs.org/@bjorn3/browser_wasi_shim/-/browser_wasi_shim-0.3.0.tgz"; flake = false; };
    ws = { url = "https://registry.npmjs.org/ws/-/ws-8.18.0.tgz"; flake = false; };
    simple-http-server = { url = "github:TheWaWaR/simple-http-server/e79ddd3cd12db97062b4a33adc2e436d0022f4be"; flake = false; };
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
          haskellPkgs = import inputs.nixpkgs-haskell {
            inherit system;
            overlays = [
              inputs.haskell-nix.overlay
              # GHC wasm patches (GHCi browser mode improvements)
              (final: prev: {
                haskell-nix = prev.haskell-nix // {
                  compiler = prev.haskell-nix.compiler // {
                    ghc9141 = prev.haskell-nix.compiler.ghc9141.override {
                      ghc-patches = prev.haskell-nix.compiler.ghc9141.patches ++
                        (with final.lib; optionals final.stdenv.targetPlatform.isWasm (
                          filter (hasSuffix ".patch") (filesystem.listFilesRecursive ./haskell/sol/ghc-wasm-patches))
                        );
                    };
                  };
                };
              })
              (final: prev: {
                hixProject =
                  final.haskell-nix.hix.project {
                    src = ./.;
                    compiler-nix-name = "ghc9141";
                    inherit evalSystem;
                    crossPlatforms = p:
                      final.lib.optionals final.stdenv.hostPlatform.isx86_64
                        # aarch64 is cross-compiled rather than built natively because
                        # `cache.zw3rk.com` has the cross GHC (`aarch64-unknown-linux-gnu-ghc-9.14.1`)
                        # but no native `aarch64-linux` paths at all, so a native build means
                        # compiling GHC itself from source under qemu.
                        [ p.wasi32 p.aarch64-multiplatform ];
                    shell.nativeBuildInputs =
                      [
                        haskellPkgs.simple-http-server
                        (
                          let
                            wasm-dummy-liblibdl = haskellPkgs.runCommand "liblibdl"
                              {
                                nativeBuildInputs = [ haskellPkgs.pkgsCross.wasi32.buildPackages.llvmPackages.clang ];
                              }
                              ''
                                mkdir -p $out/lib
                                echo 'void __liblibdl_stub(void) {}' | wasm32-unknown-wasi-cc -shared -x c - -o $out/lib/liblibdl.so 2>/dev/null
                              '';
                            forced-wasm-ghc-pkg = haskellPkgs.writeShellScriptBin "ghc-pkg" ''
                              exec wasm32-unknown-wasi-ghc-pkg "$@"
                            '';
                          in
                          # `--builddir=dist-newstyle-wasm`
                          # Cabal keys dist-newstyle/packagedb/ and dist-newstyle/cache/ on compiler-id only (ghc-9.14.1 for both toolchains), not platform, so the native and wasm builds clobber each other's sol-http-api inplace registration.
                          # should soon hopefully be solved by upstream improvements, e.g. https://github.com/haskell/cabal/pull/11179
                          haskellPkgs.writeShellScriptBin "wasm32-unknown-wasi-cabal" ''
                            PATH="${forced-wasm-ghc-pkg}/bin:$PATH" \
                            LD_LIBRARY_PATH="${wasm-dummy-liblibdl}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
                            NIX_LDFLAGS=$(echo "$NIX_LDFLAGS" | tr ' ' '\n' | grep -v 'libffi-[0-9]' | tr '\n' ' ') \
                            NIX_LDFLAGS_FOR_TARGET=$(echo "$NIX_LDFLAGS_FOR_TARGET" | tr ' ' '\n' | grep -v 'libffi-[0-9]' | tr '\n' ' ') \
                            exec cabal \
                              --builddir=dist-newstyle-wasm \
                              --with-ghc=wasm32-unknown-wasi-ghc \
                              --with-compiler=wasm32-unknown-wasi-ghc \
                              --with-ghc-pkg=wasm32-unknown-wasi-ghc-pkg \
                              --with-hsc2hs=wasm32-unknown-wasi-hsc2hs \
                              $(builtin type -P "wasm32-unknown-wasi-pkg-config" &> /dev/null && echo "--with-pkg-config=wasm32-unknown-wasi-pkg-config") \
                              "$@"
                          ''
                        )
                        (haskellPkgs.writeShellScriptBin "sol-web-build" ''
                          set -euo pipefail
                          echo "Building sol-web for wasm..."
                          wasm32-unknown-wasi-cabal build sol-web
                          rm -rf dist
                          cp -r haskell/sol/static dist
                          mkdir -p dist/assets
                          mv dist/*.css dist/assets/ 2>/dev/null || true
                          cp -r --no-preserve=mode "$BROWSER_WASI_SHIM"/dist dist/browser_wasi_shim
                          WASM_BIN=$(wasm32-unknown-wasi-cabal list-bin sol-web)
                          $(wasm32-unknown-wasi-ghc --print-libdir)/post-link.mjs --input "$WASM_BIN" --output dist/ghc_wasm_jsffi.js
                          cp "$WASM_BIN" dist/app.wasm
                          echo "Build complete. Output in dist/"
                        '')
                        (haskellPkgs.writeShellScriptBin "sol-web-serve" ''
                          set -euo pipefail
                          sol-web-build
                          echo "Serving at http://localhost:8002"
                          exec simple-http-server dist --index --nocache --open -p "8002"
                        '')
                        # TODO we'd really like to add `--enable-multi-repl sol-http-api` (or `all`)
                        # but GHCIWatch doesn't support that, as we've discovered previously
                        (haskellPkgs.writeShellScriptBin "sol-web-watch" ''
                          GHCI_BROWSER_OPEN_CMD=xdg-open \
                          ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch haskell/sol/web --debounce 50ms \
                            --watch haskell/sol/static --reload-glob '*.css' \
                            --command \
                            'wasm32-unknown-wasi-cabal repl sol-web \
                            --repl-options="-ignore-dot-ghci -fghci-browser -fghci-browser-port=8001 -fghci-browser-assets-dir=static"'
                        '')
                      ];
                    shell.tools = {
                      cabal = "latest";
                      haskell-language-server.src = inputs.hls;
                    };
                    modules = [
                      # haskell.nix #2435: wasm cross-compiler's TH interpreter crashes
                      # because `ghci` package is missing from the package DB.
                      # Workaround: stub out the TH-heavy Instances module for wasm.
                      ({ pkgs, lib, ... }: {
                        packages.generics-sop.postPatch =
                          lib.optionalString pkgs.stdenv.hostPlatform.isWasm ''
                            echo 'module Generics.SOP.Instances () where' > src/Generics/SOP/Instances.hs
                          '';
                      })
                    ];
                    shell.withHoogle = false;
                    shell.shellHook =
                      let
                        node_modules = haskellPkgs.linkFarm "node_modules" [{ name = "ws"; path = inputs.ws; }];
                      in
                      ''
                        export BROWSER_WASI_SHIM="${inputs.browser-wasi-shim}"
                        export NODE_PATH="${node_modules}''${NODE_PATH:+:$NODE_PATH}"
                        # Filter wasm cross-compilation paths from native linker flags
                        # to prevent ld.gold from choking on wasm object files.
                        # Keep libffi-wasm (needed by the wasm cabal wrapper for linking).
                        export NIX_LDFLAGS=$(echo "$NIX_LDFLAGS" | tr ' ' '\n' | grep -v -e 'wasi' -e 'compiler-rt.*wasm' -e 'libcxx.*wasm' | tr '\n' ' ')
                        export NIX_LDFLAGS_FOR_TARGET=$(echo "$NIX_LDFLAGS_FOR_TARGET" | tr ' ' '\n' | grep -v -e 'wasi' -e 'compiler-rt.*wasm' -e 'libcxx.*wasm' | tr '\n' ' ')
                      '';
                  };
              })
              # simple-http-server
              # https://github.com/TheWaWaR/simple-http-server/issues/11#issuecomment-4075592693
              (final: prev: with (import inputs.nixpkgs-unstable { inherit system; }); {
                simple-http-server = callPackage "${inputs.nixpkgs-unstable}/pkgs/by-name/si/simple-http-server/package.nix" {
                  rustPlatform = rustPlatform // {
                    buildRustPackage = args: rustPlatform.buildRustPackage (finalAttrs: args finalAttrs // {
                      version = "0.8.0";
                      src = inputs.simple-http-server;
                      cargoHash = "sha256-Ji43cp/+fEJ+z0mTIS/CnId1JP9xk9Ti0CwRRKY2saE=";
                      buildFeatures = [ "tls" ];
                    });
                  };
                };
              })
            ];
            config = inputs.haskell-nix.config;
          };
          haskell = haskellPkgs.hixProject.flake { };
          extraPackages =
            let pkgs-unstable = import inputs.nixpkgs-unstable { inherit system; config = nixpkgs-config; };
            in {
              # frequent updates are desirable
              claude-code = pkgs-unstable.claude-code;
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
            } // lib.optionalAttrs (system == "x86_64-linux") {
              # cross-compiled for the Pis - see `crossPlatforms` above, and `mkPiSdAndVm`
              # (only the exes the Pi configs actually reference - `qr` and `magic-mouse` are desktop-only)
              clark-cross-aarch64 = haskell.packages."aarch64-unknown-linux-gnu:clark:exe:clark";
              sol-cross-aarch64 = haskell.packages."aarch64-unknown-linux-gnu:sol:exe:sol";
              sol-web-dist =
                let
                  sol-web-wasm = haskell.packages."wasm32-unknown-wasi:sol:exe:sol-web";
                  wasmGhc = haskellPkgs.hixProject.projectCross.wasi32.pkg-set.config.ghc.package;
                in
                haskellPkgs.runCommand "sol-web-dist"
                  {
                    nativeBuildInputs = [ haskellPkgs.nodejs ];
                  } ''
                  mkdir -p $out/assets
                  cp ${./haskell/sol/static/index.html} $out/index.html
                  cp ${./haskell/sol/static/index.js} $out/index.js
                  cp ${./haskell/sol/static/style.css} $out/assets/style.css
                  cp -r --no-preserve=mode ${inputs.browser-wasi-shim}/dist $out/browser_wasi_shim
                  ${wasmGhc}/lib/post-link.mjs --input ${sol-web-wasm}/bin/sol-web.wasm --output $out/ghc_wasm_jsffi.js
                  cp ${sol-web-wasm}/bin/sol-web.wasm $out/app.wasm
                '';
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
        let
          pkgs-unstable-aarch64 = import inputs.nixpkgs-unstable { system = "aarch64-linux"; };
          overlays = system: [
            (_: _: extraPackages.${system}
              // { inherit (extraPackages.${buildSystem}) sol-web-dist; }
              # cross-compile from `buildSystem` rather than building natively under qemu
              # (the shadowed `extraPackages.aarch64-linux.*` are never forced)
              // lib.optionalAttrs (system == "aarch64-linux") {
              clark = extraPackages.${buildSystem}.clark-cross-aarch64;
              sol = extraPackages.${buildSystem}.sol-cross-aarch64;
            })
            # `kdePackages` comes from unstable (see `extraPackages`), so anything loaded into the
            # same process as Plasma must be built against the *same* qtbase. The only place a
            # NixOS module reads `pkgs.qt6` for that is `sddm.nix`'s Wayland greeter
            # (`pkgs.qt6.qtwayland`) - everything else Plasma-side already comes via `kdePackages`.
            # Overriding the whole `qt6`/`qt6Packages` scope instead (as we used to) also drags
            # `v4l-utils` -> `libdisplay-info` -> `mesa` off cache.nixos.org, which is a very
            # expensive aarch64 rebuild for packages that have no business caring about Plasma's Qt.
            (_: prev: { qt6 = prev.qt6 // { inherit (pkgs-unstable-aarch64.qt6) qtwayland; }; })
          ];
        in {
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
