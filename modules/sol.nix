{ pkgs, lib, ... }:
let
  # arbitrary
  evdev-share-port = 56701;
  spotifyd-port = 56702;
  sol-script-lifx-port = 56710;
  sol-script-http-port = 8000;

  # basic user service helper
  mkService =
    { atStartup ? true
    }: service:
    lib.mkMerge ([
      service
    ] ++ lib.optional atStartup {
      wantedBy = [ "default.target" ];
    });
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
  users.groups.uinput = { members = [ "gthomas" ]; };
  services.udev.extraRules = ''
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", GROUP="gpio", MODE="0660"
    KERNEL=="uinput", GROUP="uinput", MODE:="0660", OPTIONS+="static_node=uinput"
  '';

  # desktop (Plasma Bigscreen)
  services.desktopManager.plasma6.enable = true;
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
    firefox
    kdePackages.plasma-bigscreen
    vlc
  ];

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
          --lifx-timeout 4 \
          --lifx-ignore Ceiling \
          --lifx-port ${toString sol-script-lifx-port} \
          --http-port ${toString sol-script-http-port} \
          --key-send-port 56702 \
          --key-send-ips 192.168.178.20 \
          --hifi-plug-ip 192.168.178.28 \
      '';
      description = "main Haskell script";
      path = with pkgs; [ sol dbus kdePackages.qttools libgpiod ];
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
        evdev-share-server -p ${toString evdev-share-port} -n evdev-share
      '';
      path = [ pkgs.evdev-share ];
    };
  };
}
