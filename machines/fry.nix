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

  # 6.14 adds necessary support for our network card
  boot.kernelPackages = pkgs.linuxPackages_6_16;
}
