# config we want to share across all machines
{ flake
, syncCamera ? false
}:
{ pkgs, lib, config, ... }:
let
  devices = import ../nix/devices.nix;
in
{
  system.nixos.tags = [ flake.shortRev or flake.dirtyShortRev ];
  i18n.defaultLocale = "en_GB.UTF-8";
  time.timeZone = "Europe/London";

  # only Fry actually boots from ZFS (this setting used to live in `obsidian.nix` alongside the rest
  # of its ZFS config), but it has to be set for *every* machine, because the Pi configs pull in
  # `nixos/modules/profiles/base.nix` - `nixos-raspberrypi`'s `sd-image` module inherits it from
  # Nixpkgs' own `sd-image-aarch64.nix` - and that profile puts `zfs` in `boot.supportedFilesystems`
  # so that installer media can mount anything. that alone is enough to activate the ZFS module and
  # make it warn that we're relying on the default value.
  #
  # this looks like a Nixpkgs bug. the warning is emitted under `mkIf cfgZfs.enabled` (zfs.nix:666,
  # :702), where `enabled = inInitrd || inSystem`, but `forceImportRoot` is only ever *read* when
  # importing a root pool: `boot.initrd = mkIf inInitrd` (:725, :744) and `createImportService`
  # mapped over `rootPools` (:800). so it fires on any system that merely has the ZFS tools
  # available with no pool to import - which is every NixOS installer ISO and every sd-image.
  # (verified: a minimal system with an ext4 root and no ZFS whatsoever starts warning as soon as
  # `profiles/base.nix` is imported, and is silent without it.) the fix upstream would be to gate
  # the warning on `rootPools != [ ]`, or at least on `inInitrd`, rather than on `cfgZfs.enabled`.
  #
  # `false` is the new default from 26.11 anyway, and is what Fry was already set to, so moving it
  # here changes nothing for the machine where it actually does something
  boot.zfs.forceImportRoot = false;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.allow-import-from-derivation = true;
  nix.settings.trusted-public-keys = [
    "billy.george.fst-1:fgYMFJlaXSY7PVn+DTqB8xd8Difv9X4g1Repc2j77A0="
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "cache.soopy.moe-1:0RZVsQeR+GOh0VQI9rvnHz55nVXkFardDqfm4+afjPo="
    "crow.george.fst-1:vOnc1YKNNo4bQSQ+dcuzdaP3W5motYonCi2jnXGobb0="
    "fry.george.fst-1:Po60oDPTbWVr6m7IQMFBe9G1Y6y4GE6Z44KJaKAx8cY="
    "hackworthltd-private.cachix.org-1:rgRRt26yorDGvo2cu48JRE3dVPxFot/8C7L+wmiYe20="
    "hackworthltd.cachix.org-1:0JTCI0qDo2J+tonOalrSQP3yRNleN6bQucJ05yDltRI="
    "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    "haskell-pretty-simple.cachix.org-1:AWHkzPidwcDzWUIUjKcx/PYgud2OBAa9SNUEoIOsATY="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    "haskell-miso-cachix.cachix.org-1:m8hN1cvFMJtYib4tj+06xkKt5ABMSGfe8W7s40x1kQ0="
    "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
  ];
  nix.settings.substituters =
    let withPriority = i: s: "${s}?priority=${toString i}";
    in lib.imap1 withPriority [
      "https://cache.nixos.org"
      "https://cache.iog.io"
    ];
  nix.settings.trusted-substituters =
    builtins.concatMap
      (h: if h == config.networking.hostName then [ ] else [ "ssh://nix-ssh@${h}" ])
      (builtins.attrNames devices)
    ++ [
      "https://cache.zw3rk.com"
      "https://cache.soopy.moe"
      "https://hackworthltd.cachix.org"
      "https://haskell-miso-cachix.cachix.org"
      "https://haskell-language-server.cachix.org"
      "https://haskell-pretty-simple.cachix.org"
      "https://nixos-raspberrypi.cachix.org"
      "https://d1gu8ums2n7plh.cloudfront.net" # temporary Obsidian/ARIA/Reflex S3 cache
    ];
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  programs.bash.promptInit = ''
    green=$(tput setaf 10)
    blue=$(tput setaf 4)
    bold=$(tput bold)
    reset=$(tput sgr0)
    vscode=$([[ "$TERM_PROGRAM" == "vscode" ]] && echo 1 || echo 0)
    dots=$(printf '%*s' $((SHLVL - 1 - $vscode)) | tr ' ' '.')
    PS1="\[$bold\]\[$blue\]\H\[$reset\]\[$bold\]:\[$green\]\w\[$reset\]\[$bold\]\$dots\$ \[$reset\]"
  '';
  programs.bash.interactiveShellInit = ''
    bind -x '"\ec": printf "%s" "$READLINE_LINE" | wl-copy'
  '';

  # agenix
  age.secrets.wifi.file = ../secrets/wifi.age;
  age.secrets.wifi.group = "wpa_supplicant";
  age.secrets.wifi.mode = "0440";
  age.secrets.gh-key.file = ../secrets/github.key.age;
  age.secrets.mailgun-key.file = ../secrets/mailgun.key.age;
  age.secrets.mailgun-sandbox.file = ../secrets/mailgun.sandbox.age;
  age.secrets.gather-id.file = ../secrets/gather.id.age;
  age.secrets.gather-id.mode = "0444";

  services.openssh.enable = true;
  services.openssh.settings.PasswordAuthentication = false;
  users.users.gthomas.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINUnvz6Q8zIzqbIG2iy72u6zl5Xg/tem1r93G3FNwGF9 gthomas@billy"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMpgTcLz/Bu3KefOGAD2wbKybYoQBKGRDATraxQUiXMV gthomas@clark"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICqVpc5ttFcpEX4BL19nLmx4Nyl4bLvqfRBMoITUv7A1 gthomas@crow"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIENRoUCeCrR6KtXi/Trx5igMumHuDR2enXubiisk+QTE gthomas@fry"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMA9w/yrArTnqUQMKkVD6ngN5J17e7s9Eo3sKFxR3fit gthomas@sol"
  ];
  programs.git.enable = true;
  programs.git.config = {
    core.editor = "code --wait";
    pull.ff = "only";
    submodule.recurse = true;
    user.name = "George Thomas";
    user.email = "georgefsthomas@gmail.com";
  };
  environment.etc.inputrc.text = ''
    set bell-style none
    set completion-ignore-case on
    set completion-prefix-display-length 3
    set show-all-if-unmodified on
    set colored-stats on
    set visible-stats on
    set mark-symlinked-directories on
    "\e[1~": beginning-of-line
    "\e[4~": end-of-line
    "\e[5~": beginning-of-history
    "\e[6~": end-of-history
    "\e[3~": delete-char
    "\e[2~": quoted-insert
    "\e[5C": forward-word
    "\e[5D": backward-word
    "\e[1;5C": forward-word
    "\e[1;5D": backward-word
    "\e[3;5~": kill-word
    "\C-H": backward-kill-word
    "\ee": shell-expand-line
  '';
  users.users.gthomas.shell = pkgs.bash;
  environment.systemPackages = with pkgs; [
    dhall
    dhall-json
    direnv
    evtest
    file
    imagemagick
    inotify-tools
    lazygit
    live-server
    jq
    nix-direnv
    p7zip
    simple-http-server
    tree
    wasmtime
  ];
  networking.firewall.allowedUDPPorts = [
    8000
    8001
  ];
  networking.firewall.allowedTCPPorts = [
    8000
    8001
  ];
  services.syncthing =
    let
      devs = lib.filterAttrs (_: d: d ? syncthing) devices;
      devNames = builtins.attrNames devs;
    in
    {
      enable = true;
      openDefaultPorts = true;
      group = "users";
      user = "gthomas";
      dataDir = "/home/gthomas/sync";
      settings.devices = builtins.mapAttrs (_: d: d.syncthing) devs;
      settings.folders =
        builtins.mapAttrs (_: f: f // { devices = devNames; }) {
          default = {
            path = "~/sync/main";
            devices = devNames;
            versioning = {
              type = "staggered";
              params.maxAge = toString (365 * 24 * 60 * 60);
            };
          };
          fp5_bu8k-photos = {
            path = "~/sync/camera";
            label = "Android Camera";
            devices = devNames;
            enable = syncCamera;
          };
        };
    };
  system.activationScripts = {
    syncthing-root-link = ''
      if [[ ! -e /sync ]]; then ln -s /home/gthomas/sync/main /sync ; fi
    '';
  };
}
