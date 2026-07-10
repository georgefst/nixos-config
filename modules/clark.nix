{ pkgs, lib, config, ... }:
let
  # some of the places I'm using this are running as root
  home = "/home/gthomas";

  # arbitrary - all that matters is that these don't conflict with each other or anything else
  clark-script-udp-port = 56710; # if we change this we need to modify Tasker config, .bashrc etc.
  clark-script-lifx-port = 56711;
  clark-script-http-port = 8000; # if we change this we need to modify Shelly buttons etc.
  evdev-share-port = 56701;
  file-server-port = 80;
  mqtt-port = 8883; # actually the default port, and probably implicitly assumed all over, including outside this file
  extra-ports = [ 56720 ]; # for temporary scripts etc.
  pipe-dir = "/run/clark";
  system-led-pipe = "${pipe-dir}/system-led-pipe";
  power-off-pipe = "${pipe-dir}/power-off-pipe";
  email-pipe = "${pipe-dir}/email-pipe";
  notify-crash-service = "notify-crash@";

  # GPIO
  gpio-chip = 0;
  button-pin = 23;
  led-error-pin = 19;
  led-other-pin = 26;

  # helpers
  mkService =
    { asUser ? false
    , atStartup ? true
    , notifyOnCrash ? true
    , needsNetwork ? true
    }: service:
    lib.mkMerge ([
      service
      {
        postStop = ''
          printf "SERVICE_RESULT: $SERVICE_RESULT\n"
          printf "EXIT_CODE: $EXIT_CODE\n"
          printf "EXIT_STATUS: $EXIT_STATUS\n"
        '';
      }
    ] ++ lib.optional atStartup {
      wantedBy = [ "multi-user.target" ];
    } ++ lib.optional asUser {
      # we use system services everywhere to avoid issues with lingering, but sometimes need to drop down to user level
      serviceConfig.User = "gthomas";
      serviceConfig.Group = "users";
    } ++ lib.optional notifyOnCrash {
      unitConfig.OnFailure = [ "${notify-crash-service}%n.service" ];
    } ++ lib.optional needsNetwork (
      let wants = [ "network-online.target" ]; in {
        inherit wants;
        after = wants;
      }
    ));
in
{
  # stuff I'm probably never going to change
  networking.hostName = "clark";
  system.stateVersion = "26.05"; # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  hardware.enableRedistributableFirmware = true;
  hardware.firmware = [ pkgs.wireless-regdb ];

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
  networking.wireless.networks = builtins.listToAttrs
    (map (name: { inherit name; value.pskRaw = "ext:PSK_${name}"; })
      (import ../nix/wifi.nix));

  # global installs
  environment.systemPackages = with pkgs; [
    libgpiod
  ];

  # systemd
  systemd.services = {
    "${notify-crash-service}" = mkService { notifyOnCrash = false; needsNetwork = false; } {
      script = ''
        printf 'Clark service crashed: %s\nInspect service logs for more info.\n' "$1" > ${email-pipe}
      '';
      scriptArgs = "%i";
      serviceConfig.Type = "oneshot";
    };
    clark = mkService { asUser = true; } {
      script = ''
        clark \
          --gpio-chip ${toString gpio-chip} \
          --no-gpio \
          --button-debounce 0.03s \
          --button-window 1.5s \
          --button-pin ${toString button-pin} \
          --led-error-pin ${toString led-error-pin} \
          --led-other-pin ${toString led-other-pin} \
          --lifx-timeout 10 \
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
    };
    evdev-share = mkService { } {
      script = ''
        evdev-share-server -p ${toString evdev-share-port} -n evdev-share
      '';
      description = "evdev share server";
      path = [ pkgs.evdev-share ];
    };
    http-watch = mkService { } {
      script = ''
        config=/sync/config/http-watch.dhall
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
    };
    email-handler = mkService { } {
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
      serviceConfig.Restart = "always";
      description = "email handler";
      path = [ pkgs.curl ];
      # fallback: if email sending itself fails, log somewhere that will be seen
      postStop = ''
        if [ $SERVICE_RESULT != success ]
        then
          sudo -u gthomas sed -i "1iClark failed to send email at $(date)" /sync/notes/todo.md
        fi
      '';
    };
    mosquitto = mkService { } {
      script = "mosquitto -c /sync/config/mqtt/meross.conf -v";
      description = "mosquitto MQTT broker";
      path = [ pkgs.mosquitto ];
    };
    power-off = mkService { needsNetwork = false; } {
      script = ''
        data=$(<${power-off-pipe})
        echo $data
        poweroff
      '';
      description = "poweroff server";
    };
    system-leds = mkService { needsNetwork = false; } {
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
    };
    miniserve = mkService { asUser = true; } {
      script = let dir = home + "/serve"; in ''
        mkdir -p ${dir}
        miniserve \
          -u \
          -t 'Upload/download files' \
          -p ${toString file-server-port} \
          ${dir}
      '';
      serviceConfig = { AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ]; };
      description = "file server";
      path = [ pkgs.miniserve ];
    };
  };

  # open ports
  networking.firewall.allowedUDPPorts = [
    clark-script-udp-port
    clark-script-lifx-port
    evdev-share-port
  ] ++ extra-ports;
  networking.firewall.allowedTCPPorts = [
    file-server-port
    mqtt-port
    clark-script-http-port
  ] ++ extra-ports;
  # these pipes are used from multiple services, so we set them up as early as possible
  systemd.tmpfiles.rules = [
    "d ${pipe-dir} 0755 gthomas users -"
    "p ${email-pipe} 0644 gthomas users -"
    "p ${system-led-pipe} 0644 gthomas users -"
    "p ${power-off-pipe} 0644 gthomas users -"
  ];
}
