# config we want to share across all machines
{ pkgs, ... }:
{
  i18n.defaultLocale = "en_GB.UTF-8";
  time.timeZone = "Europe/London";
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.allow-import-from-derivation = true;
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "hackworthltd-private.cachix.org-1:rgRRt26yorDGvo2cu48JRE3dVPxFot/8C7L+wmiYe20="
    "hackworthltd.cachix.org-1:0JTCI0qDo2J+tonOalrSQP3yRNleN6bQucJ05yDltRI="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
    "billy.george.fst-1:fgYMFJlaXSY7PVn+DTqB8xd8Difv9X4g1Repc2j77A0="
    "crow.george.fst-1:vOnc1YKNNo4bQSQ+dcuzdaP3W5motYonCi2jnXGobb0="
    "fry.george.fst-1:Po60oDPTbWVr6m7IQMFBe9G1Y6y4GE6Z44KJaKAx8cY="
    "haskell-pretty-simple.cachix.org-1:AWHkzPidwcDzWUIUjKcx/PYgud2OBAa9SNUEoIOsATY="
    "cache.soopy.moe-1:0RZVsQeR+GOh0VQI9rvnHz55nVXkFardDqfm4+afjPo="
  ];
  nix.settings.substituters = [
    "https://cache.nixos.org"
    "https://cache.iog.io"
    "https://cache.zw3rk.com"
    "https://cache.soopy.moe"
  ];
  nixpkgs.config.allowUnfree = true;
  programs.bash.promptInit = ''
    green=$(tput setaf 10)
    blue=$(tput setaf 4)
    bold=$(tput bold)
    reset=$(tput sgr0)
    PS1="\[$bold\]\[$blue\]\H\[$reset\]\[$bold\]:\[$green\]\w\[$reset\]\[$bold\]\\$ \[$reset\]"
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
    # now this really is _user_ config, which makes the whole home-manager split look very silly
    # this is also the first thing really where I do want to share this stuff with non-Nix machines...
    # could put in separate Nix files, and parse them for non-Nix environments?
    # same goes for `programs.bash.shellInit` (or `loginShellInit` or `interactiveShellInit`)
    # can also use `bash.shellAliases` or more general `environment.shellAliases`, which take attrsets
    # https://github.com/georgefst/george-conf/blob/master/bashrc.sh
    # and to a lesser extent inputrc stuff (uncontroverially good?)
    # and then open tabs are all that remains of the "home" config I need
    user.name = "George Thomas";
    user.email = "georgefsthomas@gmail.com";
    core.editor = "code --wait";
    pull.ff = "only";
    submodule.recurse = true;
  };
  # environment.etc.inputrc = pkgs.lib.mkAfter ''
  # environment.etc.inputrc = pkgs.lib.mkForce ''
  # environment.etc.inputrc = pkgs.lib.mkForce ''
  environment.etc.inputrc.text = pkgs.lib.mkAfter ''
    set bell-style none
    set completion-ignore-case on
    set completion-prefix-display-length 3
    set show-all-if-ambiguous on
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

    # complex emacs-y things I don't use?
    # set meta-flag on
    # set input-meta on
    # set convert-meta off
    # set output-meta on

    # then add this to bash config to use alt-c to copy line
    # it's a start
    # I'd like to be able to navigate output of long-running commands without using mouse,
    # but that probably would mean ditching gnome-terminal which I otherwise like
    # this bug has been open 23 years: https://gitlab.gnome.org/GNOME/vte/-/issues/588

    # keybinding kills things...
    # bind -x '"\e\C-c": printf "%s" "$READLINE_LINE" | wl-copy'
    # doesn't work in VSCode of course
    # bind -x '"\C-p": printf "%s" "$READLINE_LINE" | wl-copy'
    # good
    # bind -x '"\ec": printf "%s" "$READLINE_LINE" | wl-copy'
  '';
  # sudo rm /etc/inputrc

  # text = pkgs.lib.mkDefault (pkgs.lib.mkAfter ''
  #   #  alternate mappings for "page up" and "page down" to search the history
  #   "\e[5~": history-search-backward
  #   "\e[6~": history-search-forward
  # '');
  # };
  users.users.gthomas.shell = pkgs.bash;
  environment.systemPackages = with pkgs; [
    file
    imagemagick
    inotify-tools
    # replace with main after my PR merge? https://github.com/lomirus/live-server/pull/176
    # maybe not since I'm not actually using it now that I discovered Epiphany watches local files
    live-server
    jq
    simple-http-server
    tree
  ];
}
