# https://nixos.org/manual/nixos/stable/options.html
# https://nixos.wiki/wiki/NixOS_on_ARM
# https://nixos.wiki/wiki/NixOS_on_ARM/Raspberry_Pi_3
{ config, pkgs, lib, ... }:
let
  secrets = import ./secrets.nix;

  # some of the places I'm using this are running as root
  home = "/home/gthomas";

  # useful for systemd `wanted-by` field, to make services always on
  startup-root = [ "multi-user.target" ];
  startup = [ "default.target" ];

  # arbitrary - all that matters is that these don't conflict with each other or anything else
  clark-script-port = 56710; # if we change this we need to modify Tasker config, .bashrc etc.
  droopy-port = 80;
  mqtt-port = 8883; # actually the default port, and probably implicitly assumed all over, including outside this file
  extra-ports = [ 56720 ]; # for temporary scripts etc.

  file-server-dir = home + "/serve";
  syncthing-main-dir = home + "/sync";
  syncthing-camera-dir = home + "/sync-camera";
in
{
  imports =
    [
    ];

  # nix stuff
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "hackworthltd-private.cachix.org-1:rgRRt26yorDGvo2cu48JRE3dVPxFot/8C7L+wmiYe20="
    "hackworthltd.cachix.org-1:0JTCI0qDo2J+tonOalrSQP3yRNleN6bQucJ05yDltRI="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
    "billy.george_____t-1:ysBHZMnX/3Gtqi1CR/3Y2qrJg7mmna1ETAM7Akyj3ek="
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

  # shell
  programs.bash.promptInit = ''
    green=$(tput setaf 10)
    blue=$(tput setaf 4)
    bold=$(tput bold)
    reset=$(tput sgr0)
    PS1="\[$bold\]\[$blue\]\H\[$reset\]\[$bold\]:\[$green\]\w\[$reset\]\[$bold\]\\$ \[$reset\]"
  '';

  # ssh
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  users.users.gthomas.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCmeBKmTzM4sOaP/JlzyL3VWYDAWn2M2IL55nC1hbaqmz5RT1zG5+LM8vzd1YHxCCdqoTqOtPi3kB0EwGQ2780BP+2zZJxw4hQdqZOuoouBFaZbo5+DHoJErj9mLETEMG3cfJYBw3GxOAn8OdUabETi7tvv3mdblzweKclR08/fECxdcdIte9CqJ9Is3T/XgsXTacl4iPUr74hDZqp1gCwq/rC5Q+cJyZHFdSpeWzUM1p5bxiSFtzB4tLd9JN6phGqFB9cuZWc3IjjEbzxbjzPs46n2oMeS8XC13LvIvkR7AY4x7rei57U1THEx2LxSvMf4bjuXzhsvF7gVRy2qILAe7hGb/6G7gF7thCyDV0z5WYCzvP3Rpj9+57dBXS99yzlaVieHeOI+ODwo6B0t/uW2jZzEnro8zA7KBbaDkBG+WaJtRDRnoWk7kN10AulCFzJgkOoAuantyWE9vM0lThPiMwkRuUgZWvNFu5xPx4rjO/skb1zxVzor/k1HmYw2d0s= gthomas@billy"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQClL6wmZoCxFf5CzwQA/IdXbw7iZ/96woUOSYETLnegj1IJ0Mcj8A6Hw/4wzU2TgmB0DqUeeIZRSwm/nMclcsKFePIWxmOF3D3CDzmhnyIGNLPp8/O45eC72Hi36TKKLyV40TeekFGnX6SGI/M3uw3TYUA0/zDlHU6aUOVsknHI3Ho+/0IesP7qKghIGx6xt8GoidiwSPSjH5QWbqLi4k4OUhmnZs5c4kGZm4AJjhh4QWXVvERzKPPIkZArmmMcq+KfGD7jq0Yp2YsnX9YnHrMRJtFqK6UYdgpvQJTM8oupaPvFM1CjBcP46b4emEBiNhMdpCzHjcBJe79VG1WWFUb0FLoyxXms+ku8c+h70tOHY4DaT+vU1CdFaUPjbsFwbAQsIquf7Vy/5sFVnlh8iWKS+l+6ctImE5jHEMwIFuHYDpZdzX/y1kSHs4z3b4qTVXfDfymX6C2LJzmDlSrEIuwLdDIBK/CaWnJaTqdk04mF4jpdSgYeQP8BOZ+RtTwxesU= gthomas@church"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC6dnDkD1einqkXi+SBKVyXYfOeJcSjKJbLmk2RmD//8Jc5DEXk31tSFachVd0Df6xGyAQt8fA7rIel30KPkzQKwwnhKP1K3E/wNBdux/ZcoN+FX4ctUMEEN8Mhs1HaC01QVVznlZEHoZPApWx82bh7pLnRZG+hVw7skcT6cZBM7VO6WyNobRtNu8wEi8ojcnL5Vq4R04QW6xm+gFGItxDhow92r3/Zl4wl5d+R6AdEiKolIjdO8a6fW8szG7HE+Nt1rpLbgDVdL3+6P9kUt4j3B0Z59erKYLO5nRZeP7afFgw/rfQZW2f2pDNnIxxihdo/L6J2YnMV5nSfsERjucTZv2DLa3NjL/Lr6toVBCcHTTk5dIFsyqjbE09MuVvCK4VTNgsI8bbGB3tMj0jdroe7ps7+nWSW8MfkZOdvpfXOm7h50XdNZ9sFWQ0fzmVCMUV4SFWAGocCnXHcvXIpIxocAfF7aND3G+QbFhLdoff9uIhrCQv089Ex1SRcrlJlKGU= u0_a447@localhost"
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
  systemd.user.services = {
    email-ip = {
      script = ''
        IP=""
        while true
        do
          NEW_IP=$(curl -s https://ipinfo.io/ip)
          if [[ $NEW_IP != $IP ]]
          then
            echo "Changed: $NEW_IP"
            curl -s --user 'api:${secrets.mailgun.key}' \
              https://api.mailgun.net/v3/sandbox${secrets.mailgun.sandbox}.mailgun.org/messages \
              -F from='Mailgun Sandbox <postmaster@sandbox${secrets.mailgun.sandbox}.mailgun.org>' \
              -F to='George Thomas <georgefsthomas@gmail.com>' \
              -F subject='Public IP address changed' \
              -F text="Old: $IP, New: $NEW_IP" \
            || sed -i "1iClark IP checker failed to send email: $(date)" ${syncthing-main-dir}/notes/todo.md
          else
            echo "No change"
          fi
          IP=$NEW_IP
          sleep $((15 * 60))
        done
      '';
      description = "notify when IP changes";
      path = [ pkgs.curl ];
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
      wantedBy = startup;
      wants = [ "geckodriver.service" ];
    };
    geckodriver = {
      script = "geckodriver";
      description = "firefox webdriver interface";
      path = [ pkgs.geckodriver pkgs.firefox ];
    };
    mosquitto = {
      script = "mosquitto -c ${syncthing-main-dir}/config/mqtt/meross.conf -v";
      description = "mosquitto MQTT broker";
      path = [ pkgs.mosquitto ];
      wantedBy = startup;
    };
  };
  # for whatever reason (e.g. binding to port 80), these need to be run as root
  systemd.services = {
    clark = {
      script = ''
        ${home}/clark \
          --gpio-chip gpiochip0 \
          --button-debounce 1 \
          --button-pin 27 \
          --led-error-pin 19 \
          --led-other-pin 26 \
          --light-name Ceiling \
          --lifx-timeout 5 \
          --receive-port ${builtins.toString clark-script-port} \
          --mailgun-sandbox ${secrets.mailgun.sandbox} \
          --mailgun-key ${secrets.mailgun.key} \
          --email-address georgefsthomas@gmail.com \
          --laptop-host-name billy \
          --ssh-timeout 3 \
          --lifx-morning-seconds 45 \
          --lifx-morning-kelvin 2700 \
          --desk-usb-port 2 \
      '';
      description = "clark script";
      path = [ pkgs.libgpiod pkgs.mosquitto pkgs.openssh ];
      wantedBy = startup-root;
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
      wantedBy = startup-root;
    };
  };

  # open ports
  networking.firewall.allowedUDPPorts = [
    clark-script-port
  ] ++ extra-ports;
  networking.firewall.allowedTCPPorts = [
    droopy-port
    mqtt-port
  ] ++ extra-ports;

  # syncthing
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = "gthomas";
    group = "users";
    dataDir = home;
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
  system.activationScripts = {
    # allows certain scripts and config files to be compatible across my devices
    syncthing-root-link.text = ''
      if [[ ! -e /syncthing ]]; then
        ln -s ${syncthing-main-dir} /syncthing
      fi
    '';
    enableLingering =
      # stops user services from being killed when all SSH sessions close
      # taken from https://github.com/NixOS/nixpkgs/issues/183629#issuecomment-1199256913
      # as discussed in that thread, there'll hopefully be a proper NixOS option for this eventually
      let touchUserLinger = username: _: "touch /var/lib/systemd/linger/${username}";
      in
      ''
        # remove all existing lingering users
        rm -rf /var/lib/systemd/linger
        mkdir -p /var/lib/systemd/linger
        # enable for the subset of declared users
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList touchUserLinger config.users.users)}
      '';

  };
}
