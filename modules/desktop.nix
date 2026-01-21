{ hostName
, stateVersion # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
, laptop ? false
, wallpaper
, syncCamera ? false
, keyboardLayout ? "gb"
}:
{ pkgs, options, ... }:
let
  gnomeExts = with pkgs.gnomeExtensions; [
    clipboard-indicator
    hide-cursor
    just-perfection
    tiling-shell
    window-calls
  ];
in
{
  networking.hostName = hostName;
  system.stateVersion = stateVersion; # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion

  # boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # desktop
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;
  programs.dconf.profiles.user.databases = [
    {
      lockAll = true;
      settings = with pkgs.lib.gvariant; {
        "desktop/ibus/panel/emoji" = {
          hotkey = [ "<Super>numbersign" ];
        };
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
          always-on-top = [ "<Super>a" ];
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
        "org/gnome/settings-daemon/plugins/media-keys" = {
          logout = [ "<Control><Super>l" ];
          reboot = [ "<Control><Super>r" ];
          shutdown = [ "<Control><Super>o" ];
        };
        "org/gnome/settings-daemon/plugins/power" = {
          power-button-action = if laptop then "suspend" else "hibernate";
          power-saver-profile-on-low-battery = false;
          ambient-enabled = false;
          idle-dim = false;
          sleep-inactive-battery-type = "nothing";
          sleep-inactive-ac-type = "nothing";
        };
        "org/gnome/shell" = {
          disabled-extensions = mkEmptyArray type.string;
          enabled-extensions = map (e: e.extensionUuid) gnomeExts;
          favorite-apps = [
            "firefox.desktop"
            "code.desktop"
            "spotify.desktop"
            "org.gnome.Geary.desktop"
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
        "org/gnome/shell/extensions/just-perfection" = {
          animation = mkUint32 4;
          double-super-to-appgrid = false;
          panel-in-overview = true;
        };
        "org/gnome/shell/extensions/tilingshell" = {
          cycle-layouts = [ "<Super>apostrophe" ];
          cycle-layouts-backward = [ "<Shift><Super>apostrophe" ];
          focus-window-down = [ "<Super>Down" ];
          focus-window-left = [ "<Super>Left" ];
          focus-window-right = [ "<Super>Right" ];
          focus-window-up = [ "<Super>Up" ];
          move-window-down = [ "<Control><Super>Down" ];
          move-window-left = [ "<Control><Super>Left" ];
          move-window-right = [ "<Control><Super>Right" ];
          move-window-up = [ "<Control><Super>Up" ];
          span-window-all-tiles = [ "<Super>backslash" ];
          span-window-down = [ "<Shift><Control><Super>Down" ];
          span-window-left = [ "<Shift><Control><Super>Left" ];
          span-window-right = [ "<Shift><Control><Super>Right" ];
          span-window-up = [ "<Shift><Control><Super>Up" ];
          untile-window = [ "<Super>z" ];
          enable-autotiling = true;
          inner-gaps = mkUint32 0;
          outer-gaps = mkUint32 0;
          layouts-json = let inherit (pkgs) lib; in builtins.toJSON (
            let
              mapWhen = p: f: map (x: if p x then f x else x);
              adjacentPairs = l: lib.zipListsWith (start: end: { inherit start end; }) l (lib.tail l);
              scanl = f: e: l:
                let
                  result = builtins.genList
                    (i: if i == 0 then e else f (builtins.elemAt result (i - 1)) (builtins.elemAt l (i - 1)))
                    (builtins.length l + 1);
                in
                result;
              boundaries = splits: adjacentPairs ([ 0 ] ++ splits ++ [ 1 ]);
              rows = rowDefs: lib.flatten (lib.zipListsWith
                (pos: map (x: { x = x.start; y = pos.start; width = x.end - x.start; height = pos.end - pos.start; }))
                (adjacentPairs (lib.init (scanl builtins.add 0 (map (row: row.height) rowDefs)) ++ [ 1 ]))
                (map (row: boundaries row.splits) rowDefs));
              cols = colDefs: lib.flatten (lib.zipListsWith
                (pos: map (y: { y = y.start; x = pos.start; height = y.end - y.start; width = pos.end - pos.start; }))
                (adjacentPairs (lib.init (scanl builtins.add 0 (map (col: col.width) colDefs)) ++ [ 1 ]))
                (map (col: boundaries col.splits) colDefs));
              grid = xSplits: ySplits:
                rows (map (y: { height = y.end - y.start; splits = xSplits; }) (boundaries ySplits));
              extendTileUp = d: tile: tile // { y = tile.y - d; height = tile.height + d; };
            in
            lib.imap
              (i: tiles: {
                id = toString i;
                tiles = map (tile: tile // { groups = [ ]; }) tiles;
              }) [
              (grid [ 0.5 ] [ ])
              (rows [
                { height = 0.68; splits = [ ]; }
                { splits = [ 0.4 ]; }
              ])
              # bottom right tile covers ugly Chromium Wayland CSD titlebar
              (mapWhen (t: t.y == 0.75) (extendTileUp 0.012) (cols [
                { width = 0.758; splits = [ ]; }
                { splits = [ 0.25 0.5 0.75 ]; }
              ])
              )
              (grid [ 0.27 ] [ ])
              (grid [ ] [ ])
              (grid [ 0.5 ] [ 0.5 ])
              # bottom left tile covers ugly Spotify Wayland CSD titlebar
              (mapWhen (t: t.x == 0 && t.y == 0.5) (extendTileUp 0.014) (grid [ 0.5 ] [ 0.5 ]))
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
          incrementBrightness = dir: name: binding: {
            name = "brightness-small-step-" + name;
            inherit binding;
            command = "${lib.getExe brightnessctl} set --exponent=2.5 2%${dir}";
          };
          bindings = lib.imap0
            (i: value: {
              name = "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom${toString i}";
              inherit value;
            })
            [
              (incrementBrightness "-" "down" "<Shift>MonBrightnessDown")
              (incrementBrightness "+" "up" "<Shift>MonBrightnessUp")
              # the bindings below are a temporary workaround, for the above not firing consistently in GNOME 49
              (incrementBrightness "-" "down-alt" "<Shift><Super>7")
              (incrementBrightness "+" "up-alt" "<Shift><Super>8")
              {
                name = "toggle-panel";
                binding = "<Super>semicolon";
                command = "${pkgs.writeShellScript "toggle-panel" ''
                  S=/org/gnome/shell/extensions/just-perfection/panel
                  if [[ $(dconf read $S) == true ]] ; then dconf write $S false ; else dconf write $S true ; fi
                ''}";
              }
            ];
        in
        { "org/gnome/settings-daemon/plugins/media-keys".custom-keybindings = map (b: "/${b.name}/") bindings; }
          // lib.listToAttrs bindings
      );
    }
  ];
  services.logind.settings.Login.HandleLidSwitch = "lock";

  # global installs
  environment.systemPackages = with pkgs;
    let
      # we take lights as arguments for now because discovery isn't working on NixOS
      # there's also a memory leak in the app, so we always close after using in practice,
      # and therefore it's actually best that we don't have to wait around for discovery when relaunching anyway!
      lifx-manager = ipParts:
        let ip = lib.concatStringsSep "." (map toString ipParts); in makeDesktopItem {
          name = "lifx-manager-${ip}";
          desktopName = "LIFX (${toString (lib.last ipParts)})";
          exec = "${lib.getExe pkgs.lifx-manager} --devices 1 --ip ${ip}";
          icon = "${../assets/lifx.png}";
          startupWMClass = "LIFX";
        };
      gather = makeDesktopItem {
        # Gather as desktop app, via Chromium
        name = "gather";
        desktopName = "Gather";
        exec = "${lib.getExe chromium} --app=https://app.v2.gather.town/app/obsidian-3812d4d3-1a3e-4e30-b603-b31c7b22e94f";
        icon = "${../assets/gather.png}";
        startupWMClass = "chrome-app.v2.gather.town__app_obsidian-3812d4d3-1a3e-4e30-b603-b31c7b22e94f-Default";
      };
      ghc = haskellPackages.ghcWithPackages (import ./haskell-libs.nix);
      vscode = vscode-with-extensions.override {
        vscode = pkgs.vscode;
        vscodeExtensions = import ./vscode-extensions.nix nix-vscode-extensions.vscode-marketplace;
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
      ghcid
      ghciwatch
      haskell-language-server
      helvum
      hix
      libreoffice
      (lifx-manager [ 192 168 178 29 ])
      (lifx-manager [ 192 168 178 30 ])
      (lifx-manager [ 192 168 178 37 ])
      nil
      nixpkgs-fmt
      obelisk
      opencode
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

  # custom services
  systemd.services.net-evdev = {
    script = ''
      ${pkgs.lib.getExe pkgs.net-evdev} \
        --port 56701 \
        --ip 192.168.178.51 \
        --switch-key KeyRightalt \
        --active-cmd '${pkgs.lib.getExe pkgs.brightnessctl} --save set 50%-' \
        --idle-cmd '${pkgs.lib.getExe pkgs.brightnessctl} --restore' \
    '';
    description = "keyboard forwarding for Pi";
    wantedBy = [ "multi-user.target" ];
  };
}
