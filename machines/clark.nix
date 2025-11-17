{ pkgs, config, ... }:
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
  ip-file = "/tmp/ip-address";

  # directories
  file-server-dir = home + "/serve";
  syncthing-main-dir = home + "/sync";
  syncthing-camera-dir = home + "/sync-camera";

  # GPIO
  gpio-chip = 0;
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
      fi
    '';
    path = (service.path or [ ]);
  };
in
{
  imports =
    [
    ];

  # stuff I'm probably never going to change
  networking.hostName = "clark";
  system.stateVersion = "21.05"; # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  hardware.enableRedistributableFirmware = true;
  hardware.firmware = [ pkgs.wireless-regdb ];

  # agenix
  age.secrets.gh-key = agenix-user-secret ../secrets/github.key.age;
  age.secrets.mailgun-key = agenix-user-secret ../secrets/mailgun.key.age;
  age.secrets.mailgun-sandbox = agenix-user-secret ../secrets/mailgun.sandbox.age;
  age.secrets.wifi = agenix-user-secret ../secrets/wifi.age;

  # overlays
  nixpkgs.overlays = [
    (self: super: { })
  ];

  # gpio and uinput permissions
  users.groups.gpio = { members = [ "gthomas" ]; };
  users.groups.uinput = { members = [ "gthomas" ]; };
  services.udev.extraRules = ''
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", GROUP="gpio", MODE="0660"
    KERNEL=="uinput", GROUP="uinput", MODE:="0660", OPTIONS+="static_node=uinput"
  '';

  # wifi
  networking.wireless.enable = true;
  networking.wireless.interfaces = [ "wlan0" ];
  networking.wireless.secretsFile = config.age.secrets.wifi.path;
  networking.wireless.networks = {
    lisa.pskRaw = "ext:PSK_lisa";
    Zeus.pskRaw = "ext:PSK_Zeus";
  };

  # ssh
  services.openssh.enable = true;

  # global installs
  environment.systemPackages = with pkgs; [
    autoPatchelfHook
    libgpiod
  ];

  # systemd
  users.users.gthomas.linger = true;
  systemd.user.services = {
    clark = service-with-crash-notification {
      script = ''
        clark \
          --gpio-chip ${toString gpio-chip} \
          --button-debounce 0.03s \
          --button-window 1.5s \
          --button-pin ${toString button-pin} \
          --led-error-pin ${toString led-error-pin} \
          --led-other-pin ${toString led-other-pin} \
          --lifx-timeout 5 \
          --lifx-port ${toString clark-script-lifx-port} \
          --receive-port ${toString clark-script-udp-port} \
          --http-port ${toString clark-script-http-port} \
          --email-pipe ${email-pipe} \
          --laptop-host-name billy \
          --ssh-timeout 3 \
          --lifx-morning-delay 45s \
          --lifx-morning-kelvin 2700 \
          --system-led-pipe ${system-led-pipe} \
          --power-off-pipe ${power-off-pipe} \
      '';
      description = "main Haskell script";
      path = [ pkgs.clark pkgs.libgpiod pkgs.mosquitto pkgs.openssh ];
      wantedBy = startup;
    };
    ip-notify = service-with-crash-notification {
      script = ''
        OLD_IP=$(cat ${ip-file} || echo undefined)
        NEW_IP=$(curl -s https://ipinfo.io/ip)
        if [[ $NEW_IP != $OLD_IP ]]
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
            # this should only happen at startup when $OLD_IP is empty
            echo "Actually, no change: $OLD_IP, $NEW_IP"
          else
            MSG="Update home IP"

            git add ssh/config
            git commit -m "$MSG"
            git push --set-upstream origin $BRANCH

            export GH_TOKEN=$(<${config.age.secrets.gh-key.path})
            OUT=$(gh pr create --title "$MSG" --body "")
            printf '%s' "$OUT"
            URL=$(printf '%s' "$OUT" | tail -n1)

            printf "Public IP address changed\n$URL" > ${email-pipe}
          fi
        else
          echo "No change"
        fi
        echo -n $NEW_IP > ${ip-file}
      '';
      serviceConfig = { Restart = "always"; RestartSec = 15 * 60; };
      description = "IP change notifier";
      path = [ pkgs.curl pkgs.gh pkgs.git pkgs.openssh ];
      wantedBy = startup;
    };
    evdev-share = service-with-crash-notification {
      script = ''
        evdev-share-server -p ${builtins.toString evdev-share-port} -n evdev-share
      '';
      description = "evdev share server";
      path = [ pkgs.evdev-share ];
      wantedBy = startup;
    };
    http-watch = service-with-crash-notification {
      script = ''
        config=/syncthing/config/http-watch.dhall
        while true
        do
          readarray -t sites < <(echo "($config).sites" | dhall-to-json | jq -c '.[]')
          for site in ''${sites[@]}
          do
            name=$(echo $site | jq -r .name)
            url=$(echo $site | jq -r .url)
            threshold=$(echo $site | jq -r .threshold)
            mkdir -p /tmp/http-watch/$name
            old=/tmp/http-watch/$name/old.html
            new=/tmp/http-watch/$name/new.html
            curl -sS $url -o $new
            d=$(diff -y --suppress-common-lines $old $new | wc -l)
            echo "Lines changed for $name: $d"
            if (( $d > $threshold ))
            then
              printf "Watched website updated: $name\n$url" > ${email-pipe}
            fi
            cp $new $old
          done
          pause=$(echo "($config).pause" | dhall)
          echo "Sleeping for $pause seconds..."
          sleep $pause
        done
      '';
      description = "HTTP watcher";
      path = [ pkgs.curl pkgs.dhall pkgs.dhall-json pkgs.diffutils pkgs.jq ];
      wantedBy = startup;
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
    settings.devices = {
      # Billy will introduce us to all others, so there's no need to list them here
      billy = {
        id = "3WIFNUH-VIST5DA-RROQ732-DDCKOQK-PWVERCB-7RNNG5R-JGRZX3M-WMAUQQP";
        introducer = true;
      };
    };
    settings.folders = {
      default = {
        path = syncthing-main-dir;
        label = "Default";
        devices = [ "billy" ];
      };
      fp5_bu8k-photos = {
        path = syncthing-camera-dir;
        label = "Android Camera";
        devices = [ "billy" ];
      };
    };
  };
  system.activationScripts = {
    # these pipes are used from multiple services, so we set them up as early as possible
    make-pipes = ''
      if [[ ! -p ${email-pipe} ]]; then mkfifo ${email-pipe} && chown gthomas:users ${email-pipe} ; fi
      if [[ ! -p ${system-led-pipe} ]]; then mkfifo ${system-led-pipe} && chown gthomas:users ${system-led-pipe} ; fi
      if [[ ! -p ${power-off-pipe} ]]; then mkfifo ${power-off-pipe} && chown gthomas:users ${power-off-pipe} ; fi
    '';
    # allows certain scripts and config files to be compatible across my devices
    syncthing-root-link = ''
      if [[ ! -e /syncthing ]]; then
        ln -s ${syncthing-main-dir} /syncthing
      fi
    '';
  };
}
