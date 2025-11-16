# stuff from Obsidian setup docs
{ pkgs, ... }: {
  networking.hostId = "69619c1a";
  boot.initrd.luks.devices.root.device = "/dev/disk/by-uuid/55f8d764-0338-4a46-a037-670137a42b63";
  boot.initrd.luks.devices.root.allowDiscards = true;
  users.extraGroups.wheel.members = [ "gthomas" ];
  services.zfs.autoScrub = {
    enable = true;
    interval = "monthly";
  };
  imports = [
    ../obsidian
    ../obsidian/users
  ];
  options.nix.settings.substituters = pkgs.lib.mkOption {
    # avoid some broken caches
    apply = pkgs.lib.filter (s: !(
      s == "s3://obsidian-open-source" ||
      pkgs.lib.hasPrefix "http://obsidian.webhop.org" s
    ));
  };
}
