# config we want to share across all machines
{ pkgs, config, extraPkgs, ... }:
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
    "haskell-pretty-simple.cachix.org-1:AWHkzPidwcDzWUIUjKcx/PYgud2OBAa9SNUEoIOsATY="
  ];
  nix.settings.substituters = [
    "https://cache.nixos.org"
    "https://cache.iog.io"
    "https://cache.zw3rk.com"
  ];
  programs.bash.promptInit = ''
    green=$(tput setaf 10)
    blue=$(tput setaf 4)
    bold=$(tput bold)
    reset=$(tput sgr0)
    PS1="\[$bold\]\[$blue\]\H\[$reset\]\[$bold\]:\[$green\]\w\[$reset\]\[$bold\]\\$ \[$reset\]"
  '';
  services.openssh.enable = true;
  services.openssh.settings.PasswordAuthentication = false;
  users.users.gthomas.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCmeBKmTzM4sOaP/JlzyL3VWYDAWn2M2IL55nC1hbaqmz5RT1zG5+LM8vzd1YHxCCdqoTqOtPi3kB0EwGQ2780BP+2zZJxw4hQdqZOuoouBFaZbo5+DHoJErj9mLETEMG3cfJYBw3GxOAn8OdUabETi7tvv3mdblzweKclR08/fECxdcdIte9CqJ9Is3T/XgsXTacl4iPUr74hDZqp1gCwq/rC5Q+cJyZHFdSpeWzUM1p5bxiSFtzB4tLd9JN6phGqFB9cuZWc3IjjEbzxbjzPs46n2oMeS8XC13LvIvkR7AY4x7rei57U1THEx2LxSvMf4bjuXzhsvF7gVRy2qILAe7hGb/6G7gF7thCyDV0z5WYCzvP3Rpj9+57dBXS99yzlaVieHeOI+ODwo6B0t/uW2jZzEnro8zA7KBbaDkBG+WaJtRDRnoWk7kN10AulCFzJgkOoAuantyWE9vM0lThPiMwkRuUgZWvNFu5xPx4rjO/skb1zxVzor/k1HmYw2d0s= gthomas@billy"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDPo6zFAS3CjRBPK2VOXjyJWsW0t/zfGEvjZKdSZLJfI4i/x4QFSUpvgXWE+zqPBlyd66K+cKjnW0Po3OgSUmfWuQwaxaTS+cNo0c65UXc3VlpM95by68gI4zSN6YdHnF3kO5pNMT6ltOij7rPos381zY9+rGrkABkwVyM4EqutVj7Bn0hhMp4zhmHm88lW5kx045dwumWasaMSu75ijmodovU8Xo1dX8q3BfXXc72OCaP+4qFu811EduVqlEwVz9ew3JpUMOjHdWnI4Ad/bA73rdRqDUN7+w1E4f1kylz6I2V4CRnQaCLSYv4bcn0reDbI5IZAYNIj7eDIDa0w8wuNC2IPhh9ujy001A/bR+rQRKignfHKLn51FXtUmDaB0tznVqfm/F777ic7Lf5CtHkszUpAk0eqK3OzawKbKPqp3A3zIadOfGETvyPzBe4au0tZmv5NXnZiDEUZfCc3ubVAI1juuCZwNjuwl49OtW1wzHsk/YEspln6W8dv3kXQgcM= gthomas@pi"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC6dnDkD1einqkXi+SBKVyXYfOeJcSjKJbLmk2RmD//8Jc5DEXk31tSFachVd0Df6xGyAQt8fA7rIel30KPkzQKwwnhKP1K3E/wNBdux/ZcoN+FX4ctUMEEN8Mhs1HaC01QVVznlZEHoZPApWx82bh7pLnRZG+hVw7skcT6cZBM7VO6WyNobRtNu8wEi8ojcnL5Vq4R04QW6xm+gFGItxDhow92r3/Zl4wl5d+R6AdEiKolIjdO8a6fW8szG7HE+Nt1rpLbgDVdL3+6P9kUt4j3B0Z59erKYLO5nRZeP7afFgw/rfQZW2f2pDNnIxxihdo/L6J2YnMV5nSfsERjucTZv2DLa3NjL/Lr6toVBCcHTTk5dIFsyqjbE09MuVvCK4VTNgsI8bbGB3tMj0jdroe7ps7+nWSW8MfkZOdvpfXOm7h50XdNZ9sFWQ0fzmVCMUV4SFWAGocCnXHcvXIpIxocAfF7aND3G+QbFhLdoff9uIhrCQv089Ex1SRcrlJlKGU= u0_a447@localhost"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICqVpc5ttFcpEX4BL19nLmx4Nyl4bLvqfRBMoITUv7A1 gthomas@crow"
  ];
  programs.git.enable = true;
  programs.git.config = {
    user.name = "George Thomas";
    user.email = "georgefsthomas@gmail.com";
  };
}
