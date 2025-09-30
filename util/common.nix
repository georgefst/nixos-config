# config we want to share across all machines
{ pkgs, ... }:
{
  i18n.defaultLocale = "en_GB.UTF-8";
  time.timeZone = "Europe/London";
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "hackworthltd-private.cachix.org-1:rgRRt26yorDGvo2cu48JRE3dVPxFot/8C7L+wmiYe20="
    "hackworthltd.cachix.org-1:0JTCI0qDo2J+tonOalrSQP3yRNleN6bQucJ05yDltRI="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
    "billy.george_____t-1:ysBHZMnX/3Gtqi1CR/3Y2qrJg7mmna1ETAM7Akyj3ek="
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
    user.name = "George Thomas";
    user.email = "georgefsthomas@gmail.com";
  };
  users.users.gthomas.shell = pkgs.bash;
}
