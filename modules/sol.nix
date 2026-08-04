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
    , needsNetwork ? false
    }: service:
    lib.mkMerge ([
      service
    ] ++ lib.optional atStartup {
      wantedBy = [ "default.target" ];
    } ++ lib.optional needsNetwork {
      # nothing orders the user manager against the network: we're started by SDDM autologin,
      # which is pulled in by `graphical.target` and races wifi association and DHCP.
      # so a service which needs the network at startup (e.g. LIFX discovery, which broadcasts
      # to 255.255.255.255 and dies with ENETUNREACH if there's no route yet) has to wait itself.
      #
      # `clark.nix`'s equivalent just sets `wants`/`after = [ "network-online.target" ]`,
      # but that target only exists in the system manager - in a user unit it's silently ignored.
      # so instead we replicate `NetworkManager-wait-online.service` by hand. `-s` is the flag
      # that unit passes, waiting for NM to log "startup complete" rather than for mere
      # connectivity, and it returns immediately once that's happened, so this costs nothing
      # outside of boot. a timeout exits non-zero, which we swallow: if the network really never
      # arrives, we'd rather start and let the service report the actual problem.
      #
      # in the long run it would probably make more sense for the Haskell script to be a *system*
      # service, as on clark, where `network-online.target` works properly and we aren't
      # reimplementing chunks of systemd's ordering (nor tying the lights and IR to a desktop
      # session). the blocker is the session bus: the `Mpris` action in `George/Core.hs` drives
      # spotifyd over it via `qdbus`/`dbus-send`, and spotifyd has to be a user service because it
      # needs the session's PulseAudio. a system service would therefore have to reach into the
      # logged-in user's bus via `DBUS_SESSION_BUS_ADDRESS`, which is fragile and reintroduces the
      # dependency on the graphical session regardless. doing it properly means system-wide audio,
      # so that spotifyd and the script can both leave the session behind.
      preStart = "${pkgs.networkmanager}/bin/nm-online -s -q -t 30 || true";
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
  networking.firewall.allowedUDPPortRanges = [
    # Steam Remote Play - peer discovery (27036) and streaming
    # https://help.steampowered.com/en/faqs/view/3E3D-BE6B-787D-A5D2
    { from = 27031; to = 27036; }
  ];
  networking.firewall.allowedTCPPorts = [
    sol-script-http-port
    spotifyd-port
    27036 # Steam Remote Play
    27037 # Steam Remote Play
  ];
  # Steam Remote Play peer discovery: the above (Valve's documented port list) is not sufficient here,
  # because it only describes the *host* side. Steam Link probes for hosts by sending a UDP broadcast
  # to 255.255.255.255:27036 from an *ephemeral* source port, and the host replies unicast from its
  # 27036 back to that port. Conntrack tracks the outgoing packet against the broadcast address, so
  # the reply - which comes from the host's own unicast address - doesn't match, isn't `ESTABLISHED`,
  # and gets dropped, leaving Steam Link showing no computers. Nothing above helps: the port that
  # needs to be open is the random one Steam Link happened to bind, so we have to match on the
  # reply's source port instead. Unicast discovery ("add computer by IP") is unaffected, and works.
  #
  # (`extraCommands`, not `extraInputRules`, since we're on the default iptables backend, not nftables.
  # these run on reload too, after `nixos-fw` is flushed, hence also the matching `extraStopCommands`)
  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -p udp --sport 27036 -j nixos-fw-accept
  '';
  networking.firewall.extraStopCommands = ''
    iptables -D nixos-fw -p udp --sport 27036 -j nixos-fw-accept || true
  '';
  users.groups.gpio = { members = [ "gthomas" ]; };
  users.groups.lirc = { members = [ "gthomas" ]; };
  users.groups.uinput = { members = [ "gthomas" ]; };
  services.udev.extraRules = ''
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", GROUP="gpio", MODE="0660"
    SUBSYSTEM=="lirc", GROUP="lirc", MODE="0660"
    KERNEL=="uinput", GROUP="uinput", MODE:="0660", OPTIONS+="static_node=uinput"
  '';
  # `uaccess` rules for game controllers, from Valve's `steam-devices`, needed by Steam Link.
  # this supersedes the three-line `udev/rules.d/56-steamlink.rules` that the app's own launcher
  # otherwise tries to `sudo cp` into `/lib/udev/rules.d` on first run (and which we drop -
  # it hands whole subsystems to `plugdev`/`input`, where these are per-device and seat-aware)
  hardware.steam-hardware.enable = true;
  # Steam Link needs to open the DMA heaps (`/dev/dma_heap/*`), which `nixos-raspberrypi`'s
  # `60-dma-heap.rules` leaves as `video`-group-only, with no `uaccess` tag and hence no ACL.
  # its video path is Valve's vendored copy of jc-kynesim's Raspberry Pi zero-copy code
  # (`testffmpeg_rpi`/`hello_wayland`/`drmu`), which installs a custom libavcodec `get_buffer2` -
  # note the `(s->codec->capabilities & AV_CODEC_CAP_DR1) != 0` assert in the binary - so frames are
  # decoded straight into dma-buf memory for import into Wayland/EGL. that applies to *software*
  # decoding too, so this isn't only about hardware decode, and without it the app segfaults on
  # `Assert( pVideoAccel )` in `streamplayer.cpp` the moment you start streaming, right after
  # logging `Effective refresh rate`, with no error of its own.
  # NB. this is broader than the `uaccess` approach preferred above, since `video` also gates every
  # DRM and V4L2 node, but unlike an ACL it doesn't depend on holding the active seat's session
  users.groups.video = { members = [ "gthomas" ]; };

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
    # TODO really there's probably a lot more from desktop.nix that we should copy here
    # this is sort of a desktop, in that it's not headless
    # but of course it isn't Gnome, and Fry and Crow are meant to be almost identical apart from hardware
    wl-clipboard
  ]
  # Valve's Raspberry Pi build of Steam Link - see `nix/steamlink.nix`, in particular for why
  # `programs.steam` (the full x86-only Steam client) isn't an option here.
  # aarch64-only, so it stays out of the x86_64 VM build (`vms.sol`) - see `mkPiSdAndVm`
  ++ lib.optional stdenv.hostPlatform.isAarch64 steamlink
  ++ map
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
    sol = mkService { needsNetwork = true; } {
      script = ''
        sol \
          --gpio-chip 0 \
          --button-pin 15 \
          --led-error-pin 5 \
          --led-idle-mode-pin 12 \
          --led-sending-mode-pin 13 \
          --led-normal-mode-pin 16 \
          --led-tv-mode-pin 6 \
          --lifx-timeout 10s \
          --lifx-retry-delay 5s \
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
