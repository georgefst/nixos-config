{ pkgs, ... }:
{
  # from Obsidian setup docs
  networking.hostId = "69619c1a";
  boot.initrd.luks.devices.root.device = "/dev/disk/by-uuid/55f8d764-0338-4a46-a037-670137a42b63";
  boot.initrd.luks.devices.root.allowDiscards = true;
  users.extraGroups.wheel.members = [ "gthomas" ];
  services.zfs.autoScrub = {
    enable = true;
    interval = "monthly";
  };

  # Gather as desktop app, via Chromium
  environment.systemPackages = with pkgs; [
    (makeDesktopItem {
      name = "gather";
      desktopName = "Gather";
      exec = "${lib.getExe chromium} --app=https://app.v2.gather.town/app/obsidian-3812d4d3-1a3e-4e30-b603-b31c7b22e94f";
      icon = "${../media/gather.png}";
    })
  ];
}
