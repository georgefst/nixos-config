{ hostName
, stateVersion # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
, laptop ? false
, wallpaper
, keyboardLayout ? "gb"
}:
{ pkgs, config, ... }:
let
  gnomeExts = with pkgs.gnomeExtensions; [
    appindicator
    clipboard-indicator
    hide-cursor
    just-perfection
    mouse-follows-focus-2
    panel-color
    tiling-shell
    window-calls
  ];
in
{
  networking.hostName = hostName;
  system.stateVersion = stateVersion; # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion

  # boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 16;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # desktop
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;
  programs.dconf.profiles.user.databases = with pkgs.lib.gvariant;
    let
      mkLoc = name: station: lat: lon: b: c: mkVariant (mkTuple [
        (mkUint32 2)
        (mkVariant (mkTuple [ name station b [ (mkTuple [ lat lon ]) ] c ]))
      ]);
      london = mkLoc "London" "EGWU" 0.89971699999999999 (-0.0072719999999999998)
        true [ (mkTuple [ 0.89971722940307675 (-0.007272211034407213) ]) ];
      cardiff = mkLoc "Cardiff" "EGFF" 0.89709923552508541 (-0.058468529941810045)
        true [ (mkTuple [ 0.89884456477707964 (-0.055850536063818547) ]) ];
      newcastle = mkLoc "Newcastle" "EGNT" 0.96051285919644858 (-0.029670597283903602)
        false
        (mkEmptyArray (type.tupleOf [ type.double type.double ]));
      zurich = mkLoc "Zürich" "LSZH" 0.82874050067087668 0.14893475701908529
        true [ (mkTuple [ 0.82670429484574492 0.14922565104551519 ]) ];
      newYork = mkLoc "New York" "KNYC" 0.71180344078725644 (-1.2909618758762367)
        true [ (mkTuple [ 0.71059804659265924 (-1.2916478949920254) ]) ];
      hongKong = mkLoc "Hong Kong" "VHHH" 0.38979019379430269 1.9928751117510946
        true [ (mkTuple [ 0.38949931722116538 1.9928751117510946 ]) ];
      clockLocations = [ newYork london zurich hongKong ];
      weatherLocations = [ london cardiff newcastle ];
      bindings = with pkgs;
        let
          incrementBrightness = dir: name: binding: {
            name = "brightness-small-step-" + name;
            inherit binding;
            command = "${lib.getExe brightnessctl} set --exponent=2.5 2%${dir}";
          };
        in
        lib.imap0
          (i: value: {
            name = "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom${toString i}";
            inherit value;
          })
          [
            (incrementBrightness "-" "down" "<Shift>MonBrightnessDown")
            (incrementBrightness "+" "up" "<Shift>MonBrightnessUp")
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
    [{
      lockAll = true;
      settings = {
        "desktop/ibus/panel/emoji" = {
          hotkey = [ "<Super>numbersign" ];
        };
        "org/gnome/clocks" = {
          world-clocks = map (l: [ (mkDictionaryEntry "location" l) ]) clockLocations;
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
          custom-keybindings = map (b: "/${b.name}/") bindings;
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
            "thunderbird.desktop"
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
        "org/gnome/shell/extensions/hide-cursor-elcste-com" = {
          timeout = mkUint32 1;
        };
        "org/gnome/shell/extensions/just-perfection" = {
          animation = mkUint32 4;
          double-super-to-appgrid = false;
          panel-in-overview = true;
        };
        "org/gnome/shell/extensions/mouse-follows-focus" = {
          bottom-bar-height = mkUint32 0;
          minimum-size-trigger = mkUint32 0;
          top-bar-height = mkUint32 0;
        };
        "org/gnome/shell/extensions/panelcolor" = {
          other-color = "rgba(0,0,0,0.5)";
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
                (map (row: boundaries (row.splits or [ ])) rowDefs));
              cols = colDefs: lib.flatten (lib.zipListsWith
                (pos: map (y: { y = y.start; x = pos.start; height = y.end - y.start; width = pos.end - pos.start; }))
                (adjacentPairs (lib.init (scanl builtins.add 0 (map (col: col.width) colDefs)) ++ [ 1 ]))
                (map (col: boundaries (col.splits or [ ])) colDefs));
              grid = xSplits: ySplits:
                rows (map (y: { height = y.end - y.start; splits = xSplits; }) (boundaries ySplits));
              uniformGrid = n: let splits = builtins.genList (i: (i + 1.0) / n) (n - 1); in grid splits splits;
            in
            lib.imap
              (i: tiles: {
                id = toString i;
                tiles = map (tile: tile // { groups = [ ]; }) tiles;
              }) [
              (grid [ 0.5 ] [ ])
              (rows [
                { height = 0.68; }
                { splits = [ 0.4 ]; }
              ])
              (cols [
                { width = 0.758; }
                { splits = [ 0.25 0.5 0.75 ]; }
              ])
              (grid [ 0.27 ] [ ])
              (uniformGrid 1)
              (uniformGrid 2)
              (uniformGrid 3)
              (uniformGrid 4)
              (uniformGrid 5)
            ]
          );
        };
        "org/gnome/shell/keybindings" = {
          toggle-application-view = [ "<Super>0" ];
          toggle-message-tray = [ "<Super>period" ];
          toggle-quick-settings = [ "<Super>comma" ];
        };
        "org/gnome/shell/weather" = {
          locations = weatherLocations;
        };
        "org/gnome/shell/world-clocks" = {
          locations = clockLocations;
        };
        "org/gnome/Weather" = {
          locations = weatherLocations;
        };
      }
      // pkgs.lib.listToAttrs bindings;
    }];
  services.logind.settings.Login.HandleLidSwitch = "lock";
  # forces electron apps to use Wayland - needed for Discord, at least, to avoid blurry text
  environment.variables.ELECTRON_OZONE_PLATFORM_HINT = "auto";

  # serve Nix store over SSH
  nix.sshServe = {
    enable = true;
    keys = config.users.users.gthomas.openssh.authorizedKeys.keys;
  };
  programs.ssh = {
    knownHosts = {
      fry.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGigM5uHEdyX7x4GXAYY5YxdYIH/3pt+XlhagfqRVtm+";
      crow.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeV081Gv4Gxxqoko//8anSYWITZV7OWL83bZM7eigmt";
    };
    extraConfig = ''
      Host fry crow
        IdentityFile /home/gthomas/.ssh/id_ed25519
      Host *
        ConnectTimeout 3
    '';
  };
  nix.settings.fallback = true;
  services.openssh.settings = {
    MaxStartups = "100:30:300";
    MaxSessions = 50;
  };
  nix.settings.secret-key-files = [ "/home/gthomas/.config/nix/private-key" ];

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
        exec = pkgs.writeShellScript "gather-launch" ''
          id=$(<${config.age.secrets.gather-id.path})
          exec ${lib.getExe chromium} --class=gather --app=https://app.v2.gather.town/app/$id;
        '';
        icon = "${../assets/gather.png}";
        startupWMClass = "gather";
      };
      ghc = haskellPackages.ghcWithPackages (import ./haskell-libs.nix pkgs);
      vscode = vscode-with-extensions.override {
        vscode = pkgs.vscode;
        vscodeExtensions = import ./vscode-extensions.nix nix-vscode-extensions.vscode-marketplace-release;
      };
    in
    [
      agenix
      crosspipe
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
      hix
      libreoffice
      (lifx-manager [ 192 168 178 29 ])
      (lifx-manager [ 192 168 178 30 ])
      (lifx-manager [ 192 168 178 37 ])
      nil
      nixd
      nixfmt
      nixpkgs-fmt
      opencode
      popsicle
      qr
      rust-analyzer
      signal-desktop
      spotify
      thunderbird
      vscode
      wl-clipboard
      ydotool
      zed-editor
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

  # firefox
  programs.firefox = {
    enable = true;
    languagePacks = [ "en-GB" ];
    preferences = {
      "browser.aboutConfig.showWarning" = false;
      "browser.tabs.closeWindowWithLastTab" = false;
      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
    };
    # N.B. default is "locked"
    # changing this makes no difference for `full-screen-api` (nor does setting it to "user", the other option)
    # but actually I suspect I might want it regardless, since I've no real reason to lock
    # note that I haven't really looked at the effect yet
    # plus, this mathces with using `defaultPref` in `autoConfig`
    # it's also kinda similar to how we configure GNOME
    preferencesStatus = "default";
    # N.B. adding `"full-screen-api.ignore-widgets" = true;` in `programs.firefox.preferences` doesn't work
    # it shows in `about:policies`, but not changed and locked in `about:config` like the others
    # this is because only preferences with certain prefixes can be configured this way:
    # https://mozilla.github.io/policy-templates/#preferences (tbf, this is in the NixOS docs for this option)
    # `autoConfig` is basically precisely for stuff not in that list
    # was implied by Claude looking at source that the difference is a matter of stability
    # the (FF, not Nix) docs say something about the first line of `autoConfig` needing to be a comment,
    # and Claude is very insistent, but we seem to get away with it
    # before we realised we could use `autoConfig`, I was exasperated and wondering whether we should fork Firefox,
    # (esp. if the other things above amount to genuine bugs), and we couldn't use some /etc config file
    # TODO maybe disable animations etc. to go with this
    # https://news.ycombinator.com/item?id=40903570
    # okay, added the two for disabling the pretty pointless warning, though that should be a separate commit
    # and I think the first isn't strictly necessary when the second is set to 0
    # the transition ones have no apparent effect - even setting to 0 doesn't change visible speed
    # TODO assuming we do add more here, can we use JS to DRY a little?
    # assuming that we'd want `defaultPref` or all options
    # or tbh, we could abstract further by using Nix
    # but then maybe that's overkill
    # probably fine, but we should be careful about showing numbers and bools
    autoConfig = ''
      defaultPref("full-screen-api.ignore-widgets", true)
      defaultPref("full-screen-api.warning.delay", 0)
      defaultPref("full-screen-api.warning.timeout", 0)
    '';
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

  # can we thing remove the git config for this?
  environment.variables = {
    EDITOR = "code --wait";
  };
}
