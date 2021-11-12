# https://nixos.org/manual/nixos/stable/options.html
# https://nixos.wiki/wiki/NixOS_on_ARM
# https://nixos.wiki/wiki/NixOS_on_ARM/Raspberry_Pi_3
{ config, pkgs, lib, ... }:
let
  secrets = import ./secrets.nix;

  # arbitrary - all that matters is that these don't conflict with each other or anything else
  clark-script-port = 56710; # if we change this we need to modify our Tasker config
  droopy-port = 8001;
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
  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  # Pi3-specific workaround: https://nixos.wiki/wiki/NixOS_on_ARM/Raspberry_Pi_3 ("WiFi / WLAN" section)
  # fixed in unstable: https://github.com/NixOS/nixpkgs/issues/101963#issuecomment-899319231
  nixpkgs.overlays = [
    (self: super: {
      firmwareLinuxNonfree = super.firmwareLinuxNonfree.overrideAttrs (old: {
        version = "2020-12-18";
        src = pkgs.fetchgit {
          url =
            "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git";
          rev = "b79d2396bc630bfd9b4058459d3e82d7c3428599";
          sha256 = "1rb5b3fzxk5bi6kfqp76q1qszivi0v1kdz1cwj2llp5sd9ns03b5";
        };
        outputHash = "1p7vn2hfwca6w69jhw5zq70w44ji8mdnibm1z959aalax6ndy146";
      });
    })
  ];
  hardware.firmware = [ pkgs.wireless-regdb ];

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
      home = "/home/gthomas";
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

  environment.systemPackages = [
    pkgs.autoPatchelfHook
    pkgs.file
    pkgs.git
    pkgs.libgpiod
    pkgs.tree
  ];

  systemd.services = {
    clark = {
      script = ''
        /home/gthomas/clark \
          --button-debounce 1 \
          --button-pin 5 \
          --light-name Ceiling \
          --lifx-timeout 5 \
          --receive-port ${builtins.toString clark-script-port} \
      '';
      description = "clark script";
      path = [ pkgs.libgpiod ]; #TODO remove once we've ported to a proper GPIO library, instead of process wrapping
      wantedBy = [ "multi-user.target" ];
    };
    droopy = {
      script = ''
        HOME=/home/gthomas droopy \
          --dl \
          -m 'Upload/download files' \
          -d /home/gthomas/serve \
          ${builtins.toString droopy-port} \
      '';
      description = "droopy file server";
      path = [ pkgs.droopy ];
      wantedBy = [ "multi-user.target" ];
    };
  };

  networking.firewall.allowedUDPPorts = [
    clark-script-port
  ];
  networking.firewall.allowedTCPPorts = [
    droopy-port
  ];
}
