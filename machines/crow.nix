{ pkgs, magic-mouse, ... }:
{
  # from official T2 Linux NixOS guide
  hardware.firmware = [
    (pkgs.stdenvNoCC.mkDerivation (final: {
      name = "brcm-firmware";
      src = ../apple-brcm;
      installPhase = ''
        mkdir -p $out/lib/firmware/brcm
        cp ${final.src}/* "$out/lib/firmware/brcm"
      '';
    }))
  ];
  networking.networkmanager.settings.main.no-auto-default = "t2_ncm";
  services.udev.extraRules = ''
    SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="ac:de:48:00:11:22", NAME="t2_ncm"
  '';

  services.openssh.enable = true;
  systemd.services.magic-mouse = {
    script = pkgs.lib.getExe magic-mouse;
    serviceConfig = { Restart = "always"; RestartSec = 1; };
    unitConfig = { StartLimitIntervalSec = 0; };
    description = "Magic mouse hack";
    wantedBy = [ "multi-user.target" ];
  };
}
