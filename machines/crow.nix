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
  environment.systemPackages = with pkgs; [
    discord
    libreoffice
  ];
  systemd.services.magic-mouse = {
    script = pkgs.lib.getExe magic-mouse;
    description = "Magic mouse hack";
    wantedBy = [ "multi-user.target" ];
  };

  # ~/code/lifx-manager/result/bin/lifx-manager --devices 2 --ip 192.168.178.37 --ip 192.168.178.29
  # ~/code/lifx-manager/result/bin/lifx-manager --devices 1
  # does any of this make any difference?
  # I can receive messages once running, but not discover
  # try the `set-ports` branch of `lifx-manager` - I think that solved something similar
  # then actually add this app to system config
  # are there any new issues having ported to Brillo?
  # memory leak: https://github.com/ad-si/Brillo/issues/16#issuecomment-2513088400
  # for some reason, spacebar requires pressing ctrl

  # rendering of all the vertical slices causes us to constantly consume 4.3% of CPU (and very possibly also explains the memory leak? though I seemed to think that had to do with Brillo and SDL)
  # we should be able to cache this, as the background never changes, but I'm not sure Gloss is clever enough
  # tbh even the base 0.6% (e.g. with `--columns 1`) is a lot for something always running that we use infrequently
  # I think maybe for now just only open the app when I need it, and aim in the medium term to move to WX or something
  networking.firewall.allowedTCPPorts = [ 56700 56701 56702 ];
  networking.firewall.allowedUDPPorts = [ 56700 56701 56702 ];
}
