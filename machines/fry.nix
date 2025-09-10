# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ pkgs, config, ... }:

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
  # no Wayland?
  # do settings declaratively? e.g. disabling screen timeout
  # _maybe_ don't need this - option may be confusingly named
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true; # just the login
  services.xserver.desktopManager.gnome.enable = true; # Patrick uses Sway - maybe see his config for audio, brightness etc.

  # global installs
  environment.systemPackages = with pkgs; [
    # or enable?
    firefox
    git
    tree
  ];

  # cherry-pick some then remove rest
  # networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # might be Gnome alrady enabling or something - wifi works anyway
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.alice = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  #   packages = with pkgs; [
  #     tree
  #   ];
  # };

  # programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  # environment.systemPackages = with pkgs; [
  #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #   wget
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # stuff from `clark.nix` that could be useful (abstract out before committing?)
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
  i18n.defaultLocale = "en_GB.UTF-8";
  time.timeZone = "Europe/London";
  # errors...
  # age.secrets.gh-key = agenix-user-secret ../secrets/github.key.age;
  # age.secrets.mailgun-key = agenix-user-secret ../secrets/mailgun.key.age;
  # age.secrets.mailgun-sandbox = agenix-user-secret ../secrets/mailgun.sandbox.age;
  # age.secrets.wifi = agenix-user-secret ../secrets/wifi.age;
  # age.secrets =
  #   let
  #     agenix-user-secret = file: {
  #       inherit file;
  #       mode = "770";
  #       owner = "gthomas";
  #       group = "users";
  #     };
  #   in
  #   {
  #     gh-key = agenix-user-secret ../secrets/github.key.age;
  #     mailgun-key = agenix-user-secret ../secrets/mailgun.key.age;
  #     mailgun-sandbox = agenix-user-secret ../secrets/mailgun.sandbox.age;
  #     wifi = agenix-user-secret ../secrets/wifi.age;
  #   };
  users.groups.uinput = { };
  # users.users = {
  #   root = { };
  #   gthomas = {
  #     isNormalUser = true;
  #     linger = true;
  #     createHome = true;
  #     home = home;
  #     extraGroups = [
  #       "gpio"
  #       "wheel"
  #       "input"
  #       "uinput"
  #     ];
  #     hashedPassword = "$6$jgaC/YaKr634BoKQ$KIv3VvRRaYShRibX5O3lAaqZ2qE3XRcYQEd0EF6YP61a9YBYUcPtljpDPE8.wEnMDNeUw9/ePBjsrK9JUv5i5/";
  #   };
  # };
  # ah, doesn't work - use Bash over ZSH?
  programs.bash.promptInit = ''
    green=$(tput setaf 10)
    blue=$(tput setaf 4)
    bold=$(tput bold)
    reset=$(tput sgr0)
    PS1="\[$bold\]\[$blue\]\H\[$reset\]\[$bold\]:\[$green\]\w\[$reset\]\[$bold\]\\$ \[$reset\]"
  '';
  services.openssh.enable = true; # maybe on by default?
  services.openssh.settings.PasswordAuthentication = false;
  users.users.gthomas.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCmeBKmTzM4sOaP/JlzyL3VWYDAWn2M2IL55nC1hbaqmz5RT1zG5+LM8vzd1YHxCCdqoTqOtPi3kB0EwGQ2780BP+2zZJxw4hQdqZOuoouBFaZbo5+DHoJErj9mLETEMG3cfJYBw3GxOAn8OdUabETi7tvv3mdblzweKclR08/fECxdcdIte9CqJ9Is3T/XgsXTacl4iPUr74hDZqp1gCwq/rC5Q+cJyZHFdSpeWzUM1p5bxiSFtzB4tLd9JN6phGqFB9cuZWc3IjjEbzxbjzPs46n2oMeS8XC13LvIvkR7AY4x7rei57U1THEx2LxSvMf4bjuXzhsvF7gVRy2qILAe7hGb/6G7gF7thCyDV0z5WYCzvP3Rpj9+57dBXS99yzlaVieHeOI+ODwo6B0t/uW2jZzEnro8zA7KBbaDkBG+WaJtRDRnoWk7kN10AulCFzJgkOoAuantyWE9vM0lThPiMwkRuUgZWvNFu5xPx4rjO/skb1zxVzor/k1HmYw2d0s= gthomas@billy"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDPo6zFAS3CjRBPK2VOXjyJWsW0t/zfGEvjZKdSZLJfI4i/x4QFSUpvgXWE+zqPBlyd66K+cKjnW0Po3OgSUmfWuQwaxaTS+cNo0c65UXc3VlpM95by68gI4zSN6YdHnF3kO5pNMT6ltOij7rPos381zY9+rGrkABkwVyM4EqutVj7Bn0hhMp4zhmHm88lW5kx045dwumWasaMSu75ijmodovU8Xo1dX8q3BfXXc72OCaP+4qFu811EduVqlEwVz9ew3JpUMOjHdWnI4Ad/bA73rdRqDUN7+w1E4f1kylz6I2V4CRnQaCLSYv4bcn0reDbI5IZAYNIj7eDIDa0w8wuNC2IPhh9ujy001A/bR+rQRKignfHKLn51FXtUmDaB0tznVqfm/F777ic7Lf5CtHkszUpAk0eqK3OzawKbKPqp3A3zIadOfGETvyPzBe4au0tZmv5NXnZiDEUZfCc3ubVAI1juuCZwNjuwl49OtW1wzHsk/YEspln6W8dv3kXQgcM= gthomas@pi"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC6dnDkD1einqkXi+SBKVyXYfOeJcSjKJbLmk2RmD//8Jc5DEXk31tSFachVd0Df6xGyAQt8fA7rIel30KPkzQKwwnhKP1K3E/wNBdux/ZcoN+FX4ctUMEEN8Mhs1HaC01QVVznlZEHoZPApWx82bh7pLnRZG+hVw7skcT6cZBM7VO6WyNobRtNu8wEi8ojcnL5Vq4R04QW6xm+gFGItxDhow92r3/Zl4wl5d+R6AdEiKolIjdO8a6fW8szG7HE+Nt1rpLbgDVdL3+6P9kUt4j3B0Z59erKYLO5nRZeP7afFgw/rfQZW2f2pDNnIxxihdo/L6J2YnMV5nSfsERjucTZv2DLa3NjL/Lr6toVBCcHTTk5dIFsyqjbE09MuVvCK4VTNgsI8bbGB3tMj0jdroe7ps7+nWSW8MfkZOdvpfXOm7h50XdNZ9sFWQ0fzmVCMUV4SFWAGocCnXHcvXIpIxocAfF7aND3G+QbFhLdoff9uIhrCQv089Ex1SRcrlJlKGU= u0_a447@localhost"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICqVpc5ttFcpEX4BL19nLmx4Nyl4bLvqfRBMoITUv7A1 gthomas@crow"
  ];
  # networking.wireless.enable = true;
  # networking.wireless.interfaces = [ "wlan0" ];
  # networking.wireless.secretsFile = config.age.secrets.wifi.path;
  # networking.wireless.networks = {
  #   lisa.pskRaw = "ext:PSK_lisa";
  #   Zeus.pskRaw = "ext:PSK_Zeus";
  # };
  # programs.git.enable = true;
  # programs.git.config = {
  #   user.name = "George Thomas";
  #   user.email = "georgefsthomas@gmail.com";
  # };

  # maybe look in to nushell, fish, better cd
}
