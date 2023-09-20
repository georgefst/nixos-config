{ pkgs, config, extraPkgs, ... }:
with builtins;
let
  # some of the places I'm using this are running as root
  home = "/home/gthomas";

  # useful for systemd `wanted-by` field, to make services always on
  startup-root = [ "multi-user.target" ];
  startup = [ "default.target" ];

  # arbitrary - all that matters is that these don't conflict with each other or anything else
  clark-script-udp-port = 56710; # if we change this we need to modify Tasker config, .bashrc etc.
  clark-script-lifx-port = 56711;
  clark-script-http-port = 8000; # if we change this we need to modify Shelly buttons etc.
  evdev-share-port = 56701;
  droopy-port = 80;
  mqtt-port = 8883; # actually the default port, and probably implicitly assumed all over, including outside this file
  extra-ports = [ 56720 ]; # for temporary scripts etc.
  system-led-pipe = "/tmp/system-led-pipe";
  power-off-pipe = "/tmp/power-off-pipe";
  email-pipe = "/tmp/email-pipe";

  # directories
  file-server-dir = home + "/serve";
  syncthing-main-dir = home + "/sync";
  syncthing-camera-dir = home + "/sync-camera";

  # GPIO
  gpiochip = "gpiochip0";
  button-pin = 23;
  led-error-pin = 19;
  led-other-pin = 26;

  # helpers
  agenix-user-secret = file: {
    inherit file;
    mode = "770";
    owner = "gthomas";
    group = "users";
  };
  service-with-crash-notification = service: service // {
    postStop = ''
      printf "SERVICE_RESULT: $SERVICE_RESULT\n"
      printf "EXIT_CODE: $EXIT_CODE\n"
      printf "EXIT_STATUS: $EXIT_STATUS\n"
    '' + (service.postStop or "") + ''
      if [ $SERVICE_RESULT != success ]
      then
        printf 'Clark service crashed: ${service.description}
        Inspect service logs for more info.
        ' > ${email-pipe}
        gpioset --mode=signal ${gpiochip} ${toString led-error-pin}=1
      fi
    '';
    serviceConfig = (service.serviceConfig or { }) // {
      TimeoutStopSec = "infinity"; # we want `gpioset` to persist until we manually restart the service
    };
  };
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
    "billy.george.fst-1:fgYMFJlaXSY7PVn+DTqB8xd8Difv9X4g1Repc2j77A0="
  ];
  nix.settings.substituters = [
    "https://cache.nixos.org"
    "https://cache.iog.io"
    "https://cache.zw3rk.com"
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

  # agenix
  age.secrets.gh-key = agenix-user-secret ../secrets/github.key.age;
  age.secrets.mailgun-key = agenix-user-secret ../secrets/mailgun.key.age;
  age.secrets.mailgun-sandbox = agenix-user-secret ../secrets/mailgun.sandbox.age;
  age.secrets.passwords-lta = agenix-user-secret ../secrets/passwords.lta.age;
  age.secrets.wifi = agenix-user-secret ../secrets/wifi.age;

  # overlays
  nixpkgs.overlays = [
    (self: super: { })
  ];

  # gpio
  users.groups.gpio = { };
  services.udev.extraRules = ''
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", GROUP="gpio", MODE="0660"
    KERNEL=="uinput", GROUP="uinput", MODE:="0660", OPTIONS+="static_node=uinput"
  '';

  # users
  users.groups.uinput = { };
  users.users = {
    root = { };
    gthomas = {
      isNormalUser = true;
      createHome = true;
      home = home;
      extraGroups = [
        "gpio"
        "wheel"
        "input"
        "uinput"
      ];
      hashedPassword = "$6$jgaC/YaKr634BoKQ$KIv3VvRRaYShRibX5O3lAaqZ2qE3XRcYQEd0EF6YP61a9YBYUcPtljpDPE8.wEnMDNeUw9/ePBjsrK9JUv5i5/";
    };
  };

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
  services.openssh.settings.PasswordAuthentication = false;
  users.users.gthomas.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCmeBKmTzM4sOaP/JlzyL3VWYDAWn2M2IL55nC1hbaqmz5RT1zG5+LM8vzd1YHxCCdqoTqOtPi3kB0EwGQ2780BP+2zZJxw4hQdqZOuoouBFaZbo5+DHoJErj9mLETEMG3cfJYBw3GxOAn8OdUabETi7tvv3mdblzweKclR08/fECxdcdIte9CqJ9Is3T/XgsXTacl4iPUr74hDZqp1gCwq/rC5Q+cJyZHFdSpeWzUM1p5bxiSFtzB4tLd9JN6phGqFB9cuZWc3IjjEbzxbjzPs46n2oMeS8XC13LvIvkR7AY4x7rei57U1THEx2LxSvMf4bjuXzhsvF7gVRy2qILAe7hGb/6G7gF7thCyDV0z5WYCzvP3Rpj9+57dBXS99yzlaVieHeOI+ODwo6B0t/uW2jZzEnro8zA7KBbaDkBG+WaJtRDRnoWk7kN10AulCFzJgkOoAuantyWE9vM0lThPiMwkRuUgZWvNFu5xPx4rjO/skb1zxVzor/k1HmYw2d0s= gthomas@billy"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDPo6zFAS3CjRBPK2VOXjyJWsW0t/zfGEvjZKdSZLJfI4i/x4QFSUpvgXWE+zqPBlyd66K+cKjnW0Po3OgSUmfWuQwaxaTS+cNo0c65UXc3VlpM95by68gI4zSN6YdHnF3kO5pNMT6ltOij7rPos381zY9+rGrkABkwVyM4EqutVj7Bn0hhMp4zhmHm88lW5kx045dwumWasaMSu75ijmodovU8Xo1dX8q3BfXXc72OCaP+4qFu811EduVqlEwVz9ew3JpUMOjHdWnI4Ad/bA73rdRqDUN7+w1E4f1kylz6I2V4CRnQaCLSYv4bcn0reDbI5IZAYNIj7eDIDa0w8wuNC2IPhh9ujy001A/bR+rQRKignfHKLn51FXtUmDaB0tznVqfm/F777ic7Lf5CtHkszUpAk0eqK3OzawKbKPqp3A3zIadOfGETvyPzBe4au0tZmv5NXnZiDEUZfCc3ubVAI1juuCZwNjuwl49OtW1wzHsk/YEspln6W8dv3kXQgcM= gthomas@pi"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC6dnDkD1einqkXi+SBKVyXYfOeJcSjKJbLmk2RmD//8Jc5DEXk31tSFachVd0Df6xGyAQt8fA7rIel30KPkzQKwwnhKP1K3E/wNBdux/ZcoN+FX4ctUMEEN8Mhs1HaC01QVVznlZEHoZPApWx82bh7pLnRZG+hVw7skcT6cZBM7VO6WyNobRtNu8wEi8ojcnL5Vq4R04QW6xm+gFGItxDhow92r3/Zl4wl5d+R6AdEiKolIjdO8a6fW8szG7HE+Nt1rpLbgDVdL3+6P9kUt4j3B0Z59erKYLO5nRZeP7afFgw/rfQZW2f2pDNnIxxihdo/L6J2YnMV5nSfsERjucTZv2DLa3NjL/Lr6toVBCcHTTk5dIFsyqjbE09MuVvCK4VTNgsI8bbGB3tMj0jdroe7ps7+nWSW8MfkZOdvpfXOm7h50XdNZ9sFWQ0fzmVCMUV4SFWAGocCnXHcvXIpIxocAfF7aND3G+QbFhLdoff9uIhrCQv089Ex1SRcrlJlKGU= u0_a447@localhost"
  ];

  # wifi
  networking.wireless.enable = true;
  networking.wireless.interfaces = [ "wlan0" ];
  networking.wireless.environmentFile = config.age.secrets.wifi.path;
  networking.wireless.networks = {
    TNCAPA620AF.psk = "@PSK_TNCAPA620AF@";
    RML-5ghz.psk = "@PSK_RML-5ghz@";
    GT.psk = "@PSK_GT@";
  };

  # git
  programs.git.enable = true;
  programs.git.config = {
    user.name = "George Thomas";
    user.email = "georgefsthomas@gmail.com";
  };

  # global installs
  environment.systemPackages = with pkgs; [
    autoPatchelfHook
    file
    libgpiod
    tree
  ];

  # systemd
  systemd.user.services = {
    clark = service-with-crash-notification {
      script = ''
        clark \
          --gpio-chip ${gpiochip} \
          --button-debounce 0.3s \
          --button-window 1.5s \
          --button-pin ${toString button-pin} \
          --led-error-pin ${toString led-error-pin} \
          --led-other-pin ${toString led-other-pin} \
          --ceiling-light-name Ceiling \
          --lamp-name Lamp \
          --lifx-timeout 5 \
          --lifx-port ${toString clark-script-lifx-port} \
          --receive-port ${toString clark-script-udp-port} \
          --http-port ${toString clark-script-http-port} \
          --email-pipe ${email-pipe} \
          --laptop-host-name billy \
          --ssh-timeout 3 \
          --lifx-morning-delay 45s \
          --lifx-morning-kelvin 2700 \
          --desk-usb-port 2 \
          --system-led-pipe ${system-led-pipe} \
          --power-off-pipe ${power-off-pipe} \
      '';
      description = "main Haskell script";
      path = [ extraPkgs.clark pkgs.libgpiod pkgs.mosquitto pkgs.openssh ];
      wantedBy = startup;
    };
    ip-notify = service-with-crash-notification {
      script = ''
        MSG="Update home IP"
        IP=""
        NEW_IP=$(
          curl -s https://ipinfo.io/ip ||
            printf "Public IP address lookup failed\nWill try again on next iteration." > ${email-pipe}
        )
        if [[ $NEW_IP != $IP ]]
        then
          echo "Changed: $NEW_IP"

          DIR=$(mktemp -d)
          cd $DIR
          git clone git@github.com:georgefst/george-conf
          cd george-conf

          BRANCH=clark-ip-$(date +%s)
          git switch -c $BRANCH
          sed -i -e "0,/HostName.*/s//HostName $NEW_IP/" ssh/config # NB. this assumes home is the first in the file

          if [[ ! `git status --porcelain` ]]
          then
            # this should only happen at startup when $IP is empty
            echo "Actually, no change: $IP, $NEW_IP"
          else
            git add ssh/config
            git commit -m "$MSG"
            git push --set-upstream origin $BRANCH

            export GH_TOKEN=$(<${config.age.secrets.gh-key.path})
            URL=$(gh pr create --title "$MSG" --body "" | tee /dev/tty | tail -n1)

            printf "Public IP address changed\n$URL" > ${email-pipe}
          fi
        else
          echo "No change"
        fi
        IP=$NEW_IP
        sleep $((15 * 60))
      '';
      serviceConfig = { Restart = "always"; };
      description = "IP change notifier";
      path = [ pkgs.curl pkgs.gh pkgs.git pkgs.openssh ];
      wantedBy = startup;
    };
    evdev-share = service-with-crash-notification {
      script = ''
        evdev-share-server -p ${builtins.toString evdev-share-port} -n evdev-share
      '';
      description = "evdev share server";
      path = [ extraPkgs.evdev-share ];
      wantedBy = startup;
    };
    tennis-scraper = service-with-crash-notification {
      script = ''
        tennis-scraper \
          --username georgefst \
          --password $(<${config.age.secrets.passwords-lta.path}) \
          --dhall ${home}/sync/config/tennis-scraper.dhall \
          --notify ${
            pkgs.writeShellScript "notify" ''
              printf "$1\n$2" > ${email-pipe}
            ''
          } \
          --headless \
          --wait-multiplier 3 \
          --failure-limit 10 \
      '';
      description = "tennis scraper";
      path = [ pkgs.curl extraPkgs.tennis-scraper ];
      wantedBy = startup;
      wants = [ "geckodriver.service" ];
    };
    email-handler = {
      script = ''
        data=$(<${email-pipe})
        subject=$(head -n1 <<< "$data")
        body=$(tail -n+2 <<< "$data")
        echo "Sending: $subject"
        curl --user "api:$(<${config.age.secrets.mailgun-key.path})" \
          https://api.mailgun.net/v3/sandbox$(<${config.age.secrets.mailgun-sandbox.path}).mailgun.org/messages \
          -F from="Mailgun Sandbox <postmaster@sandbox$(<${config.age.secrets.mailgun-sandbox.path}).mailgun.org>" \
          -F to='George Thomas <georgefsthomas@gmail.com>' \
          -F subject="$subject" \
          -F text="$body" \
      '';
      postStop = ''
        if [ $SERVICE_RESULT != success ]
        then
          sed -i "1iClark failed to send email at $(date)" ${syncthing-main-dir}/notes/todo.md
        fi
      '';
      serviceConfig = { Restart = "always"; };
      description = "email handler";
      path = [ pkgs.curl ];
      wantedBy = startup;
    };
    geckodriver = service-with-crash-notification {
      script = "geckodriver";
      description = "firefox webdriver interface";
      path = [ pkgs.geckodriver pkgs.firefox ];
    };
    mosquitto = service-with-crash-notification {
      script = "mosquitto -c ${syncthing-main-dir}/config/mqtt/meross.conf -v";
      description = "mosquitto MQTT broker";
      path = [ pkgs.mosquitto ];
      wantedBy = startup;
    };
  };
  # for whatever reason (e.g. binding to port 80), these need to be run as root
  systemd.services = {
    power-off = service-with-crash-notification {
      script = ''
        data=$(<${power-off-pipe})
        echo $data
        poweroff
      '';
      description = "poweroff server";
      wantedBy = startup-root;
    };
    system-leds = service-with-crash-notification {
      script = ''
        data=$(<${system-led-pipe})
        echo $data
        if [[ $data == 0 ]]
        then
          echo none > /sys/class/leds/mmc1::/trigger
          echo none > /sys/class/leds/ACT/trigger
        else
          echo mmc1 > /sys/class/leds/mmc1::/trigger
          echo heartbeat > /sys/class/leds/ACT/trigger
        fi
      '';
      serviceConfig = { Restart = "always"; };
      description = "system led server";
      wantedBy = startup-root;
    };
    droopy = service-with-crash-notification {
      script = ''
        mkdir -p ${file-server-dir}
        HOME=${home} droopy \
          --dl \
          -m 'Upload/download files' \
          -d ${file-server-dir} \
          ${toString droopy-port} \
      '';
      description = "droopy file server";
      path = [ pkgs.droopy ];
      wantedBy = startup-root;
    };
    rotate-video-output = service-with-crash-notification {
      script = ''
        sleep 30 # usually requires < 10, but err on the safe side - it's never really a serious bottleneck in practice
        echo 3 > /sys/class/graphics/fbcon/rotate
      '';
      description = "rotate video output";
      wantedBy = startup-root;
    };
  };

  # open ports
  networking.firewall.allowedUDPPorts = [
    clark-script-udp-port
    clark-script-lifx-port
    evdev-share-port
  ] ++ extra-ports;
  networking.firewall.allowedTCPPorts = [
    droopy-port
    mqtt-port
    clark-script-http-port
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
        id = "3WIFNUH-VIST5DA-RROQ732-DDCKOQK-PWVERCB-7RNNG5R-JGRZX3M-WMAUQQP";
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
    # these pipes are used from multiple services, so we set them up as early as possible
    make-pipes = ''
      if [[ ! -e ${email-pipe} ]]; then mkfifo ${email-pipe} && chown gthomas:users ${email-pipe} ; fi
      if [[ ! -e ${system-led-pipe} ]]; then mkfifo ${system-led-pipe} && chown gthomas:users ${system-led-pipe} ; fi
      if [[ ! -e ${power-off-pipe} ]]; then mkfifo ${power-off-pipe} && chown gthomas:users ${power-off-pipe} ; fi
    '';
    # allows certain scripts and config files to be compatible across my devices
    syncthing-root-link = ''
      if [[ ! -e /syncthing ]]; then
        ln -s ${syncthing-main-dir} /syncthing
      fi
    '';
    # ensures correct orientation on the portrait monitor usually used
    # stops user services from being killed when all SSH sessions close
    # inspired by https://github.com/NixOS/nixpkgs/issues/183629#issuecomment-1199256913
    # as discussed in that thread, there'll hopefully be a proper NixOS option for this eventually
    enable-lingering = ''
      rm -rf /var/lib/systemd/linger
      mkdir -p /var/lib/systemd/linger
      touch /var/lib/systemd/linger/gthomas
    '';
  };
}
