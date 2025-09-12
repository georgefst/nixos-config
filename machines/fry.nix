# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ pkgs, ... }:

let
  gnomeExts = with pkgs; [
    gnomeExtensions.tiling-shell
  ];
in

{
  # stuff that will probably never change
  networking.hostName = "fry";
  networking.hostId = "69619c1a";
  system.stateVersion = "25.05"; # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  boot.initrd.luks.devices.root.device = "/dev/disk/by-uuid/55f8d764-0338-4a46-a037-670137a42b63";
  boot.initrd.luks.devices.root.allowDiscards = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  users.extraGroups.wheel.members = [ "gthomas" ];
  services.pipewire = {
    enable = true;
    audio.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };
  services.zfs.autoScrub = {
    enable = true;
    interval = "monthly";
  };

  # desktop
  # note that we aren't actually using X (just bad naming): https://github.com/NixOS/nixpkgs/issues/94799
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  programs.dconf.profiles.user.databases = [
    {
      lockAll = true;
      settings = with pkgs.lib.gvariant; {
        "org/gnome/desktop/input-sources" = {
          sources = [ (mkTuple [ "xkb" "gb" ]) ];
        };
        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
        };
        "org/gnome/desktop/session" = {
          idle-delay = mkUint32 0;
        };
        "org/gnome/settings-daemon/plugins/power" = {
          power-saver-profile-on-low-battery = false;
          idle-dim = false;
          sleep-inactive-battery-type = "nothing";
          sleep-inactive-ac-type = "nothing";
        };
        "org/gnome/shell" = {
          disabled-extensions = mkEmptyArray type.string;
          enabled-extensions = map (e: e.extensionUuid) gnomeExts;
        };
        "org/gnome/shell/app-switcher" = {
          current-workspace-only = true;
        };
      };
    }
  ];

  # global installs
  environment.systemPackages = with pkgs; [
    firefox
    nil
    nixpkgs-fmt
    spotify
    tree
    (vscode-with-extensions.override {
      inherit vscode;
      vscodeExtensions = with nix-vscode-extensions.vscode-marketplace; [
        arrterian.nix-env-selector
        asuka.insertnumbers
        brunnerh.insert-unicode
        gruntfuggly.todo-tree
        haskell.haskell
        janw4ld.lambda-black
        jnoortheen.nix-ide
        jsynowiec.vscode-insertdatestring
      ];
    })
    (makeDesktopItem {
      name = "gather";
      desktopName = "Gather";
      exec = "${lib.getExe chromium} --app=https://app.gather.town/app/BMa0PDnHghjBlmqU/obsidiansystems";
    })
  ] ++ gnomeExts;

  # syncthing
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    group = "users";
    user = "gthomas";
    dataDir = "/home/gthomas/sync";
    settings.devices = {
      billy = {
        id = "3WIFNUH-VIST5DA-RROQ732-DDCKOQK-PWVERCB-7RNNG5R-JGRZX3M-WMAUQQP";
        introducer = true;
      };
    };
    settings.folders = {
      default = {
        path = "~/sync/main";
        devices = [ "billy" ];
      };
      fp5_bu8k-photos = {
        path = "~/sync/camera";
        label = "Android Camera";
        devices = [ "billy" ];
        enable = false;
      };
    };
  };

  system.activationScripts = {
    syncthing-root-link = "if [[ ! -e /sync ]]; then ln -s /home/gthomas/sync/main /sync ; fi";
  };
}
