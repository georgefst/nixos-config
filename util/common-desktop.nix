{ hostName
, stateVersion # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
, laptop ? false
, wallpaper
, syncCamera ? false
, keyboardLayout ? "gb"
}:
{ pkgs, options, ... }:
let
  gnomeExts = with pkgs; [
    gnomeExtensions.clipboard-indicator
    gnomeExtensions.hide-cursor
    gnomeExtensions.tiling-shell
    gnomeExtensions.window-calls
  ];
in
{
  networking.hostName = hostName;
  system.stateVersion = stateVersion; # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion

  # boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # desktop
  services.xserver = {
    # note that we aren't actually using X (just bad naming): https://github.com/NixOS/nixpkgs/issues/94799
    enable = true;
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };
  programs.dconf.profiles.user.databases = [
    {
      lockAll = true;
      settings = with pkgs.lib.gvariant; {
        "org/gnome/desktop/background" = {
          picture-uri-dark = "file:///${wallpaper}";
        };
        "org/gnome/desktop/input-sources" = {
          sources = [ (mkTuple [ "xkb" keyboardLayout ]) ];
        };
        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
        };
        "org/gnome/desktop/peripherals/mouse" = {
          natural-scroll = true;
        };
        "org/gnome/desktop/session" = {
          idle-delay = mkUint32 0;
        };
        "org/gnome/desktop/wm/keybindings" = {
          always-on-top = [ "<Super>s" ];
          begin-resize = [ "<Super>r" ];
          close = [ "<Super>space" ];
          switch-input-source = [ "<Alt><Super>space" ];
          switch-input-source-backward = [ "<Shift><Alt><Super>space" ];
          toggle-on-all-workspaces = [ "<Super>s" ];
        };
        "org/gnome/mutter" = {
          experimental-features = [ "scale-monitor-framebuffer" ];
          workspaces-only-on-primary = true;
        };
        "org/gnome/settings-daemon/plugins/color" = {
          night-light-enabled = true;
          night-light-temperature = mkUint32 3500;
        };
        "org/gnome/settings-daemon/plugins/power" = {
          power-button-action = if laptop then "suspend" else "hibernate";
          power-saver-profile-on-low-battery = false;
          idle-dim = false;
          sleep-inactive-battery-type = "nothing";
          sleep-inactive-ac-type = "nothing";
        };
        "org/gnome/shell" = {
          disabled-extensions = mkEmptyArray type.string;
          enabled-extensions = map (e: e.extensionUuid) gnomeExts;
          favorite-apps =
            [
              "firefox.desktop"
              "code.desktop"
              "spotify.desktop"
              "geary.desktop"
              "org.gnome.Calendar.desktop"
              "org.gnome.Nautilus.desktop"
              "org.gnome.Console.desktop"
              "org.gnome.Settings.desktop"
              "gather.desktop"
            ];
        };
        "org/gnome/shell/app-switcher" = {
          current-workspace-only = true;
        };
        "org/gnome/shell/extensions/clipboard-indicator" = {
          toggle-menu = [ "<Super>c" ];
        };
        "org/gnome/shell/extensions/tilingshell" = {
          untile-window = [ "<Super>z" ];
          span-window-all-tiles = [ "<Super>backslash" ];
          focus-window-right = [ "<Super>Right" ];
          focus-window-left = [ "<Super>Left" ];
          focus-window-up = [ "<Super>Up" ];
          focus-window-down = [ "<Super>Down" ];
          move-window-right = [ "<Control><Super>Right" ];
          move-window-left = [ "<Control><Super>Left" ];
          move-window-up = [ "<Control><Super>Up" ];
          move-window-down = [ "<Control><Super>Down" ];
          span-window-right = [ "<Shift><Control><Super>Right" ];
          span-window-left = [ "<Shift><Control><Super>Left" ];
          span-window-up = [ "<Shift><Control><Super>Up" ];
          span-window-down = [ "<Shift><Control><Super>Down" ];
          enable-autotiling = true;
          inner-gaps = mkUint32 0;
          outer-gaps = mkUint32 0;
          layouts-json = let inherit (pkgs) lib; in builtins.toJSON (
            let
              grid = xSplits: ySplits:
                let adjacentPairs = l: lib.zipListsWith (start: end: { inherit start end; }) l (lib.tail l); in
                lib.mapCartesianProduct
                  ({ x, y }: { x = x.start; y = y.start; width = x.end - x.start; height = y.end - y.start; })
                  (builtins.mapAttrs (_: l: adjacentPairs ([ 0 ] ++ l ++ [ 1 ])) { x = xSplits; y = ySplits; });
            in
            lib.imap
              (i: tiles: {
                id = toString i;
                tiles = map (tile: tile // { groups = [ ]; }) tiles;
              }) [
              (grid [ 0.5 ] [ ])
              [
                { x = 0; y = 0; width = 1; height = 0.68; }
                { x = 0; y = 0.68; width = 0.4; height = 0.32; }
                { x = 0.4; y = 0.68; width = 0.6; height = 0.32; }
              ]
              [
                { x = 0; y = 0; width = 0.853; height = 1; }
                { x = 0.853; y = 0; width = 0.147; height = 0.158; }
                { x = 0.853; y = 0.158; width = 0.147; height = 0.842; }
              ]
              (grid [ 0.27 ] [ ])
              (grid [ 0.5 ] [ 0.5 ])
              ([
                # bottom left tile covers ugly Spotify Wayland CSD titlebar
                { height = 0.5; width = 0.5; x = 0; y = 0; }
                { height = 0.5 + 0.02; width = 0.5; x = 0; y = 0.5 - 0.02; }
                { height = 0.5; width = 0.5; x = 0.5; y = 0; }
                { height = 0.5; width = 0.5; x = 0.5; y = 0.5; }
              ])
              (grid [ 0.33333 0.66667 ] [ 0.33333 0.66667 ])
              (grid [ 0.25 0.50 0.75 ] [ 0.25 0.50 0.75 ])
              (grid [ 0.2 0.4 0.6 0.8 ] [ 0.2 0.4 0.6 0.8 ])
            ]
          );
        };
        "org/gnome/shell/keybindings" = {
          toggle-application-view = [ "<Super>0" ];
          toggle-message-tray = [ "<Super>period" ];
          toggle-quick-settings = [ "<Super>comma" ];
        };
      } // (with pkgs;
        let
          bindings = lib.imap0
            (i: value: {
              name = "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom${toString i}";
              inherit value;
            })
            [
              {
                name = "brightness-small-step-down";
                binding = "<Shift>MonBrightnessDown";
                command = "${lib.getExe brightnessctl} set --exponent=2 2%-";
              }
              {
                name = "brightness-small-step-up";
                binding = "<Shift>MonBrightnessUp";
                command = "${lib.getExe brightnessctl} set --exponent=2 +2%";
              }
            ];
        in
        { "org/gnome/settings-daemon/plugins/media-keys".custom-keybindings = map (b: "/${b.name}/") bindings; }
          // lib.listToAttrs bindings
      );
    }
  ];
  services.logind =
    if pkgs.lib.hasAttrByPath [ "services" "logind" "settings" ] options
    then { settings.Login = { HandleLidSwitch = "ignore"; }; }
    else { extraConfig = "HandleLidSwitch=ignore"; };
  # forces electron apps to use Wayland - needed for VSCode, at least, to avoid blurry text
  environment.variables.ELECTRON_OZONE_PLATFORM_HINT = "auto";

  # global installs
  environment.systemPackages = with pkgs;
    let
      gather = makeDesktopItem {
        # Gather as desktop app, via Chromium
        name = "gather";
        desktopName = "Gather";
        exec = "${lib.getExe chromium} --app=https://app.v2.gather.town/app/obsidian-3812d4d3-1a3e-4e30-b603-b31c7b22e94f";
        icon = "${../media/gather.png}";
      };
      ghc = haskellPackages.ghcWithPackages (import ./haskell-libs.nix);
      spotify = pkgs.spotify.overrideAttrs (old: {
        # https://community.spotify.com/t5/Desktop-Linux/Wayland-support/td-p/5231525/page/6
        # when fixed, we can also remove `tilingshell` overlapping layout
        nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.makeWrapper ];
        postInstall = (old.postInstall or "") + "wrapProgram $out/bin/spotify --add-flags --ozone-platform=wayland";
      });
      vscode = vscode-with-extensions.override {
        vscode = pkgs.vscode;
        vscodeExtensions = (import ./vscode-extensions.nix nix-vscode-extensions.vscode-marketplace);
      };
    in
    [
      dhall-lsp-server
      discord
      element-desktop
      eyedropper
      fourmolu
      gather
      ghc
      ghciwatch
      haskell-language-server
      libreoffice
      nil
      nixpkgs-fmt
      popsicle
      rust-analyzer
      signal-desktop
      spotify
      vscode
      wl-clipboard
      ydotool
    ]
    ++ gnomeExts
    ++ [
      # https://github.com/arrterian/nix-env-selector/issues/95
      (pkgs.writeShellScriptBin "nix-shell-vscode" ''
        if [[ "$*" == *"--run export"* ]]; then
          nix-shell "$@" | grep -v '^declare -x TMP=' | grep -v '^declare -x TMPDIR='
        else
          exec nix-shell "$@"
        fi
      '')
    ];
  fonts.packages = with pkgs; [
    hasklig
  ];
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # firefox
  programs.firefox = {
    enable = true;
    languagePacks = [ "en-GB" ];
    preferences = {
      "browser.tabs.closeWindowWithLastTab" = false;
      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
    };
  };

  # pipewire
  services.pipewire = {
    enable = true;
    audio.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # syncthing
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    group = "users";
    user = "gthomas";
    dataDir = "/home/gthomas/sync";
    settings.devices = {
      billy = {
        id = "3WIFNUH-VIST5DA-RROQ732-DDCKOQK-PWVERCB-7RNNG5R-JGRZX3M-WMAUQQP";
        introducer = true;
      };
    };
    settings.folders = {
      default = {
        path = "~/sync/main";
        devices = [ "billy" ];
      };
      fp5_bu8k-photos = {
        path = "~/sync/camera";
        label = "Android Camera";
        devices = [ "billy" ];
        enable = syncCamera;
      };
    };
  };
  system.activationScripts.syncthing-root-link = ''
    if [[ ! -e /sync ]]; then ln -s /home/gthomas/sync/main /sync ; fi
  '';
}
