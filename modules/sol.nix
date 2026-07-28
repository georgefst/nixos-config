{ pkgs, lib, ... }:
let
  # arbitrary
  evdev-share-port = 56701;
  spotifyd-port = 56702;
  sol-script-lifx-port = 56710;
  sol-script-http-port = 8000;
  evdev-share-name = "Remote";

  # basic user service helper
  mkService =
    { atStartup ? true
    }: service:
    lib.mkMerge ([
      service
    ] ++ lib.optional atStartup {
      wantedBy = [ "default.target" ];
    });

  # Firefox web apps ("taskbar tabs"), managed declaratively
  #
  # why use this feature at all, rather than plain bookmarks or the kiosk-window hack (cf. `gather` in desktop.nix)?
  # - each site runs in a dedicated window: no tab bar, slim toolbar, and scope enforcement -
  #   in-scope links stay in the app window, anything else opens in the normal browser
  #   (unlike `--kiosk`, where external links are a mess)
  # - each window gets its own identity (app id matches the desktop entry name),
  #   so apps appear in the Bigscreen launcher and task switcher as themselves, with their own icons, not as "Firefox"
  # - all apps share the one Firefox profile, so logins, cookies and DRM state just work -
  #   no per-app profile creation and maintenance like `gather` needs
  #
  # when a site is pinned via the Firefox UI (with `browser.taskbarTabs.enabled`), it creates exactly two artifacts,
  # which we instead generate from the list below:
  # - `taskbartabs/taskbartabs.json` in the profile, registering each app's id, scope and start URL
  # - a `.desktop` entry in `~/.local/share/applications`, whose `Exec` looks the app up by id
  #   (ours go in the system profile instead - Firefox only reads its recorded shortcut path when unpinning,
  #   which the read-only json rules out anyway, so nothing ever looks for them in the home directory)
  # the ids are arbitrary UUIDs - iPlayer's is preserved from when it was pinned manually,
  # and the others come from `uuidgen`
  # NB. since we symlink the json read-only, pinning/unpinning via the Firefox UI will no longer work
  # (Firefox saves the registry with a plain non-atomic write, so it fails harmlessly against /etc)
  #
  # we select the profile by name (`-P`), unlike the `-profile <path>` Firefox itself bakes into shortcuts,
  # because the profile's directory name is salted and random, whereas its name is stable -
  # this saves us hardcoding per-install state, at the cost of a slight divergence from Firefox's own format
  # explicit selection one way or the other is essential, since implicit default-profile resolution
  # depends on mutable state in `profiles.ini` (and Firefox's flaky per-install hashing),
  # so could silently land the web apps (and their registry lookups) in the wrong profile
  firefoxProfileName = "default";
  webApps = [
    { id = "b4b073df-4461-4c3c-91c4-d7459a35b2a7"; name = "BBC iPlayer"; url = "https://www.bbc.co.uk/iplayer"; hostname = "www.bbc.co.uk"; icon = ../assets/webapp-icons/bbc-iplayer.png; }
    { id = "c6014a26-c9b2-494a-b529-f0c3cd4361b7"; name = "YouTube"; url = "https://www.youtube.com"; hostname = "www.youtube.com"; icon = ../assets/webapp-icons/youtube.png; }
    { id = "de1c7718-3390-4960-bd76-c820be70374b"; name = "Netflix"; url = "https://www.netflix.com"; hostname = "www.netflix.com"; icon = ../assets/webapp-icons/netflix.png; }
    { id = "8f8e4872-47f0-4d83-848f-0c066adc9abf"; name = "Channel 4"; url = "https://www.channel4.com"; hostname = "www.channel4.com"; icon = ../assets/webapp-icons/channel4.png; }
    { id = "ac1b7698-0586-4a19-b630-6b129160b531"; name = "ITVX"; url = "https://www.itv.com/watch"; hostname = "www.itv.com"; icon = ../assets/webapp-icons/itvx.png; }
  ];
  webAppDesktopFile = app: "org.mozilla.firefox.webapp-${app.id}.desktop";
in
{
  # basics
  networking.hostName = "sol";
  system.stateVersion = "26.05";
  networking.networkmanager.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
  networking.firewall.allowedUDPPorts = [
    sol-script-lifx-port
    evdev-share-port
    spotifyd-port
    5353 # mDNS
  ];
  networking.firewall.allowedTCPPorts = [
    sol-script-http-port
    spotifyd-port
  ];
  users.groups.gpio = { members = [ "gthomas" ]; };
  users.groups.lirc = { members = [ "gthomas" ]; };
  users.groups.uinput = { members = [ "gthomas" ]; };
  services.udev.extraRules = ''
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", GROUP="gpio", MODE="0660"
    SUBSYSTEM=="lirc", GROUP="lirc", MODE="0660"
    KERNEL=="uinput", GROUP="uinput", MODE:="0660", OPTIONS+="static_node=uinput"
  '';

  # desktop (Plasma Bigscreen)
  services.desktopManager.plasma6.enable = true;
  # pulls in the whole legacy KF5 stack (~30 derivations) via `plasma6.nix`'s `kio5-plugins-only`,
  # which is never cached because our unstable `kdePackages` shifts `extra-cmake-modules`.
  # nothing here is Qt5 - Bigscreen is Qt6, Firefox is GTK.
  services.desktopManager.plasma6.enableQt5Integration = false;
  services.displayManager = {
    sddm = {
      enable = true;
      wayland.enable = true;
    };
    sessionPackages = [
      pkgs.kdePackages.plasma-bigscreen
    ];
    defaultSession = "plasma-bigscreen-wayland";
    autoLogin = {
      enable = true;
      user = "gthomas";
    };
  };
  programs.kdeconnect.enable = true;
  environment.etc."xdg/kwalletrc".text = ''
    [Wallet]
    Enabled=false
  '';

  # programs
  environment.systemPackages = with pkgs; [
    kdePackages.plasma-bigscreen
    vlc
  ] ++ map
    (app: pkgs.makeDesktopItem {
      # `Exec` matches the format Firefox itself generates when pinning
      name = lib.removeSuffix ".desktop" (webAppDesktopFile app);
      desktopName = app.name;
      icon = app.icon;
      exec = ''"/run/current-system/sw/bin/firefox" "-P" "${firefoxProfileName}" "-taskbar-tab" "${app.id}" "-new-window" "${app.url}" "-container" "0"'';
    })
    webApps;
  programs.firefox = {
    enable = true;
    preferences = {
      "browser.taskbarTabs.enabled" = true;
    };
  };

  # firefox web apps - see comment on `webApps` above
  environment.etc."xdg/firefox-web-apps/taskbartabs.json".text = builtins.toJSON {
    version = 1;
    taskbarTabs = map
      (app: {
        inherit (app) id name;
        scopes = [{ inherit (app) hostname; prefix = "/"; }];
        userContextId = 0;
        startUrl = app.url;
        shortcutRelativePath = webAppDesktopFile app;
      })
      webApps;
  };
  system.activationScripts.firefox-web-apps = {
    # ensures `install -d` below doesn't create root-owned dirs in the home directory - see comment in universal.nix
    deps = [ "xdg-hack-symlinks" ];
    text = ''
      # resolve the profile's salted directory name at runtime - see comment on `firefoxProfileName` above
      # if the profile doesn't exist yet (fresh install, Firefox never run), skip:
      # the json will appear on the first switch/boot after Firefox creates its profile
      # NB. assumes `IsRelative=1`, which holds for any profile Firefox created itself
      firefox_dir=/home/gthomas/.config/mozilla/firefox
      profile=$(awk -F= '
        /^\[/ { name=""; path="" }
        $1=="Name" { name=$2 }
        $1=="Path" { path=$2 }
        name=="${firefoxProfileName}" && path!="" { print path; exit }
      ' "$firefox_dir/profiles.ini" 2>/dev/null || true)
      if [[ -n "$profile" ]]; then
        install -d -o gthomas -g users "$firefox_dir/$profile/taskbartabs"
        ln -sf /etc/xdg/firefox-web-apps/taskbartabs.json "$firefox_dir/$profile/taskbartabs/taskbartabs.json"
      fi
    '';
  };

  # custom services
  systemd.user.services = {
    sol = mkService { } {
      script = ''
        sol \
          --gpio-chip 0 \
          --button-pin 15 \
          --led-error-pin 5 \
          --led-idle-mode-pin 12 \
          --led-sending-mode-pin 13 \
          --led-normal-mode-pin 16 \
          --led-tv-mode-pin 6 \
          --lifx-timeout 10 \
          --lifx-port ${toString sol-script-lifx-port} \
          --http-port ${toString sol-script-http-port} \
          --web-root ${pkgs.sol-web-dist} \
          --keyboard-names ${evdev-share-name} \
          --key-send-port 56702 \
          --key-send-ips 192.168.178.20 \
          --hifi-plug-ip 192.168.178.28 \
          --ir-config-dir ${../assets/ir}
      '';
      description = "main Haskell script";
      path = with pkgs; [ sol dbus kdePackages.qttools libgpiod v4l-utils ];
    };
    spotifyd = mkService { } {
      description = "Spotify daemon";
      serviceConfig = {
        Restart = "always";
        RestartSec = 5;
      };
      script = ''
        spotifyd --no-daemon \
          -B320 \
          -b pulseaudio \
          --device-type avr \
          --max-cache-size ${toString (5 * 1024 * 1024 * 1024)} \
          --zeroconf-port ${toString spotifyd-port} \
          -d sol
      '';
      path = [ pkgs.spotifyd ];
    };
    evdev-share = mkService { } {
      description = "evdev share server";
      script = ''
        evdev-share-server -p ${toString evdev-share-port} -n ${evdev-share-name}
      '';
      path = [ pkgs.evdev-share ];
    };
  };
}
