{ pkgs, lib, ... }:
let
  # arbitrary
  evdev-share-port = 56701;
  spotifyd-port = 56702;

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
  hardware = {
    raspberry-pi.config.all.dt-overlays.hifiberry-dacplusdsp.enable = true;
    raspberry-pi.config.all.base-dt-params.audio.enable = lib.mkForce false;
    raspberry-pi.config.all.dt-overlays.vc4-kms-v3d.params.noaudio.enable = true;
  };
  networking.networkmanager.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
  networking.firewall.allowedUDPPorts = [
    evdev-share-port
    spotifyd-port
    5353 # mDNS
  ];
  networking.firewall.allowedTCPPorts = [
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
