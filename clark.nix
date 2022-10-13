# https://nixos.org/manual/nixos/stable/options.html
# https://nixos.wiki/wiki/NixOS_on_ARM
# https://nixos.wiki/wiki/NixOS_on_ARM/Raspberry_Pi_3
{ config, pkgs, lib, ... }:
let
  secrets = import ./secrets.nix;

  # some of the places I'm using this are running as root
  home = "/home/gthomas";

  # useful for systemd `wanted-by` field, to make services always on
  startup = [ "multi-user.target" ];

  # arbitrary - all that matters is that these don't conflict with each other or anything else
  clark-script-port = 56710; # if we change this we need to modify Tasker config, .bashrc etc.
  droopy-port = 80;
  extra-ports = [ 56720 ]; # for temporary scripts etc.

  file-server-dir = home + "/serve";
  syncthing-main-dir = home + "/sync";
  syncthing-camera-dir = home + "/camera-sync";
in
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  # stuff I'm probably never going to change
  networking.hostName = "clark";
  system.stateVersion = "21.05"; # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  i18n.defaultLocale = "en_GB.UTF-8";
  time.timeZone = "Europe/London";
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  hardware.enableRedistributableFirmware = true;
  hardware.firmware = [ pkgs.wireless-regdb ];
  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  # enable access via clark.local address
  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
      userServices = true;
      workstation = true;
    };
  };

  # overlays
  nixpkgs.overlays = [
    (self: super: { })
  ];

  # gpio
  users.groups.gpio = { };
  services.udev.extraRules = ''
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", GROUP="gpio", MODE="0660"
  '';

  # users
  users.users = {
    root = { };
    gthomas = {
      isNormalUser = true;
      createHome = true;
      home = home;
      extraGroups = [
        "gpio"
        "wheel"
      ];
      hashedPassword = "$6$jgaC/YaKr634BoKQ$KIv3VvRRaYShRibX5O3lAaqZ2qE3XRcYQEd0EF6YP61a9YBYUcPtljpDPE8.wEnMDNeUw9/ePBjsrK9JUv5i5/";
    };
  };
  security.sudo.wheelNeedsPassword = false;

  # ssh
  services.openssh.enable = true;
  users.users.gthomas.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCmeBKmTzM4sOaP/JlzyL3VWYDAWn2M2IL55nC1hbaqmz5RT1zG5+LM8vzd1YHxCCdqoTqOtPi3kB0EwGQ2780BP+2zZJxw4hQdqZOuoouBFaZbo5+DHoJErj9mLETEMG3cfJYBw3GxOAn8OdUabETi7tvv3mdblzweKclR08/fECxdcdIte9CqJ9Is3T/XgsXTacl4iPUr74hDZqp1gCwq/rC5Q+cJyZHFdSpeWzUM1p5bxiSFtzB4tLd9JN6phGqFB9cuZWc3IjjEbzxbjzPs46n2oMeS8XC13LvIvkR7AY4x7rei57U1THEx2LxSvMf4bjuXzhsvF7gVRy2qILAe7hGb/6G7gF7thCyDV0z5WYCzvP3Rpj9+57dBXS99yzlaVieHeOI+ODwo6B0t/uW2jZzEnro8zA7KBbaDkBG+WaJtRDRnoWk7kN10AulCFzJgkOoAuantyWE9vM0lThPiMwkRuUgZWvNFu5xPx4rjO/skb1zxVzor/k1HmYw2d0s= gthomas@billy"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQClL6wmZoCxFf5CzwQA/IdXbw7iZ/96woUOSYETLnegj1IJ0Mcj8A6Hw/4wzU2TgmB0DqUeeIZRSwm/nMclcsKFePIWxmOF3D3CDzmhnyIGNLPp8/O45eC72Hi36TKKLyV40TeekFGnX6SGI/M3uw3TYUA0/zDlHU6aUOVsknHI3Ho+/0IesP7qKghIGx6xt8GoidiwSPSjH5QWbqLi4k4OUhmnZs5c4kGZm4AJjhh4QWXVvERzKPPIkZArmmMcq+KfGD7jq0Yp2YsnX9YnHrMRJtFqK6UYdgpvQJTM8oupaPvFM1CjBcP46b4emEBiNhMdpCzHjcBJe79VG1WWFUb0FLoyxXms+ku8c+h70tOHY4DaT+vU1CdFaUPjbsFwbAQsIquf7Vy/5sFVnlh8iWKS+l+6ctImE5jHEMwIFuHYDpZdzX/y1kSHs4z3b4qTVXfDfymX6C2LJzmDlSrEIuwLdDIBK/CaWnJaTqdk04mF4jpdSgYeQP8BOZ+RtTwxesU= gthomas@church"
  ];

  # wifi
  networking.wireless.enable = true;
  networking.wireless.interfaces = [ "wlan0" ];
  networking.wireless.networks = secrets.wifi;

  # global installs
  environment.systemPackages = with pkgs; [
    autoPatchelfHook
    file
    git
    libgpiod
    tree
  ];

  # systemd
  systemd.services = {
    clark = {
      script = ''
        ${home}/clark \
          --button-debounce 1 \
          --button-pin 27 \
          --led-error-pin 4 \
          --led-other-pin 14 \
          --light-name Ceiling \
          --lifx-timeout 5 \
          --receive-port ${builtins.toString clark-script-port} \
          --mailgun-sandbox ${secrets.mailgun.sandbox} \
          --mailgun-key ${secrets.mailgun.key} \
      '';
      description = "clark script";
      path = [ pkgs.libgpiod ]; #TODO remove once we've ported to a proper GPIO library, instead of process wrapping
      wantedBy = startup;
    };
    droopy = {
      script = ''
        mkdir -p ${file-server-dir}
        HOME=${home} droopy \
          --dl \
          -m 'Upload/download files' \
          -d ${file-server-dir} \
          ${builtins.toString droopy-port} \
      '';
      description = "droopy file server";
      path = [ pkgs.droopy ];
      wantedBy = startup;
    };
    tennis-scraper = {
      script = ''
        ${home}/tennis-scraper \
          --username georgefst \
          --password ${secrets.passwords.lta} \
          --dhall ${home}/sync/config/tennis-scraper.dhall \
          --notify ${
            pkgs.writeShellScript "notify" ''
              curl -s --user 'api:${secrets.mailgun.key}' \
                https://api.mailgun.net/v3/sandbox${secrets.mailgun.sandbox}.mailgun.org/messages \
                -F from='Mailgun Sandbox <postmaster@sandbox${secrets.mailgun.sandbox}.mailgun.org>' \
                -F to='George Thomas <georgefsthomas@gmail.com>' \
                -F subject="$1" \
                -F text="$2" \
              || sed -i "1iClark tennis scraper failed to send email: $(date)" ${syncthing-main-dir}/notes/todo.md
            ''
          } \
          --headless \
          --wait-multiplier 3 \
      '';
      description = "tennis scraper";
      path = [ pkgs.curl ];
      environment = {
        # for Dhall - we probably wouldn't need this if we weren't running as root
        XDG_CACHE_HOME = "${home}/.cache";
      };
      wantedBy = startup;
    };
    geckodriver = {
      script = "geckodriver";
      description = "firefox webdriver interface";
      path = [ pkgs.geckodriver pkgs.firefox ];
      wantedBy = startup;
    };
  };

  # open ports
  networking.firewall.allowedUDPPorts = [
    clark-script-port
  ] ++ extra-ports;
  networking.firewall.allowedTCPPorts = [
    droopy-port
  ] ++ extra-ports;

  # syncthing
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = "gthomas";
    group = "users";
    dataDir = home;
    declarative = {
      devices = {
        # Billy will introduce us to all others, so there's no need to list them here
        billy = {
          id = "SNTZHCK-IRIPPLQ-4QHR7T6-PCTRUZ3-TVGFDJS-RJOE5VP-GJQI43B-TFQ7GQM";
          introducer = true;
        };
      };
      folders = {
        default = {
          path = syncthing-main-dir;
          label = "Default";
          devices = [ "billy" ];
        };
        fp3_4j86-photos = {
          path = syncthing-camera-dir;
          label = "Android Camera";
          devices = [ "billy" ];
        };
      };
    };
  };
}
