{ pkgs, ... }:
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
  environment.systemPackages = with pkgs;  [
    discord
    libreoffice
  ];
}
