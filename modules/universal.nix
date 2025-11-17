# config we want to share across all machines
{ flake }:
{ pkgs, lib, ... }:
{
  system.nixos.tags = [ flake.shortRev or flake.dirtyShortRev ];
  i18n.defaultLocale = "en_GB.UTF-8";
  time.timeZone = "Europe/London";
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.allow-import-from-derivation = true;
  nix.settings.trusted-public-keys = [
    "billy.george.fst-1:fgYMFJlaXSY7PVn+DTqB8xd8Difv9X4g1Repc2j77A0="
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "cache.soopy.moe-1:0RZVsQeR+GOh0VQI9rvnHz55nVXkFardDqfm4+afjPo="
    "crow.george.fst-1:vOnc1YKNNo4bQSQ+dcuzdaP3W5motYonCi2jnXGobb0="
    "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE="
    "fry.george.fst-1:Po60oDPTbWVr6m7IQMFBe9G1Y6y4GE6Z44KJaKAx8cY="
    "hackworthltd-private.cachix.org-1:rgRRt26yorDGvo2cu48JRE3dVPxFot/8C7L+wmiYe20="
    "hackworthltd.cachix.org-1:0JTCI0qDo2J+tonOalrSQP3yRNleN6bQucJ05yDltRI="
    "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    "haskell-pretty-simple.cachix.org-1:AWHkzPidwcDzWUIUjKcx/PYgud2OBAa9SNUEoIOsATY="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
  ];
  nix.settings.substituters = lib.imap0 (i: s: "${s}?priority=${toString i}") [
    "https://cache.nixos.org"
    "https://cache.iog.io"
    "https://cache.zw3rk.com"
    "https://cache.soopy.moe"
    "https://hackworthltd.cachix.org"
    "https://haskell-miso-cachix.cachix.org"
    "https://haskell-language-server.cachix.org"
    "https://haskell-pretty-simple.cachix.org"
    "https://digitallyinduced.cachix.org"
  ];
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  programs.bash.promptInit = ''
    green=$(tput setaf 10)
    blue=$(tput setaf 4)
    bold=$(tput bold)
    reset=$(tput sgr0)
    dots=$(printf '%*s' $((SHLVL - 1)) | tr ' ' '.')
    PS1="\[$bold\]\[$blue\]\H\[$reset\]\[$bold\]:\[$green\]\w\[$reset\]\[$bold\]\$dots\$ \[$reset\]"
  '';
  programs.bash.interactiveShellInit = ''
    bind -x '"\ec": printf "%s" "$READLINE_LINE" | wl-copy'
  '';
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
    evtest
    file
    imagemagick
    inotify-tools
    lazygit
    live-server
    jq
    simple-http-server
    tree
  ];
  networking.firewall.allowedUDPPorts = [
    8000
    8001
  ];
  networking.firewall.allowedTCPPorts = [
    8000
    8001
  ];
}
