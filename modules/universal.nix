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
  # now this really is _user_ config, which makes the whole home-manager split look very silly
  # this is also the first thing really where I do want to share this stuff with non-Nix machines...
  # could put in separate Nix files, and parse them for non-Nix environments?
  # same goes for `programs.bash.shellInit` (or `loginShellInit` or `interactiveShellInit`)
  # can also use `bash.shellAliases` or more general `environment.shellAliases`, which take attrsets
  # https://github.com/georgefst/george-conf/blob/master/bashrc.sh
  # and to a lesser extent inputrc stuff (uncontroverially good?)
  # and then open tabs are all that remains of the "home" config I need

  # put everything in here? i.e. the `bind` as well?
  # and do the same for `.inputrc`?
  programs.bash.interactiveShellInit = ''
    bind -x '"\ec": printf "%s" "$READLINE_LINE" | wl-copy'
  '' + builtins.readFile ../assets/bash-shortcuts.sh;

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
    core.excludesFile = "/etc/gitignore";
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
    # TODO match order to below?
    # TODO for the "same as GHC" ones, we could maybe DRY as it's very mechanical
    xdg-hack-symlinks = ''
      # open issue (tab already open on Crow, as well as for Fourmolu) - use XDG
      ln -sf /etc/xdg/hlint/hlint.yaml /home/gthomas/.hlint.yaml

      # open issue - respect XDG dirs rather than just config home
      mkdir -p /home/gthomas/.config/ghc
      ln -sf /etc/xdg/ghc/ghci.conf /home/gthomas/.config/ghc/ghci.conf
      # also, try to duplicate my `.inputrc` config? https://github.com/haskell/haskeline/wiki/UserPreferences

      # open issue - should be in config rather than data for a start (is Firefox the same (now it supports XDG)?)
      mkdir -p /home/gthomas/.local/share/epiphany
      ln -sf /etc/xdg/epiphany/user-stylesheet.css /home/gthomas/.local/share/epiphany/user-stylesheet.css

      # open issue - same as GHC
      for i in $(ls /etc/xdg/evolution/sources)
      do
        ln -sf /etc/xdg/evolution/sources/$i /home/gthomas/.config/evolution/sources/$i
      done

      # open issue - same as GHC
      mkdir -p /home/gthomas/.config/gtk-4.0
      ln -sf /etc/xdg/gtk-4.0/gtk.css /home/gthomas/.config/gtk-4.0/gtk.css

      # this part is just a test
      # I actually should check whether any of my apps are using GTK 3
      # and whether there's any particular harm in enabling this
      mkdir -p /home/gthomas/.config/gtk-3.0
      ln -sf /etc/xdg/gtk-4.0/gtk.css /home/gthomas/.config/gtk-3.0/gtk.css

      mkdir -p /home/gthomas/.config/opencode
      mkdir -p /home/gthomas/.config/opencode/plugins
      ln -sf /etc/xdg/opencode/opencode.jsonc /home/gthomas/.config/opencode/opencode.jsonc
      ln -sf /etc/xdg/opencode/plugins/notification.js /home/gthomas/.config/opencode/plugins/notification.js
    '';
  };
  environment.etc."gitignore".source = ../assets/gitignore;
  # use Gnome support for holidays and contact birthdays instead of Gmail?
  # somehow use separate colour for repeating reminders? maybe create the calendar directly instead of using Gmail...
  environment.etc."xdg/hlint/hlint.yaml".source = ../assets/hlint.yaml;
  # this is unfortunately very similar to the Gmail calendar colour, which I don't think I can change
  environment.etc."xdg/evolution/sources/cardiff-city.source".source = ../assets/calendars/cardiff-city.source;
  environment.etc."xdg/evolution/sources/wales-football.source".source = ../assets/calendars/wales-football.source;
  environment.etc."xdg/evolution/sources/wales-rugby.source".source = ../assets/calendars/wales-rugby.source;
  environment.etc."xdg/evolution/sources/work.source".text =
    lib.replaceString
      "ResourcePath"
      # should be an agenix secret
      "ResourcePath=/owa/calendar/7611458db6af4410ba5fc88cba92e3b8@obsidian.systems/f85aabad4ba74634ad276e7f4b9117c41206466590018772298/S-1-8-3577668476-3458094751-683252461-3825177555/reachcalendar.ics"
      (builtins.readFile ../assets/calendars/work.source);
  # set GMail as default calendar (currently set imperatively)
  # dconf write /org/gnome/evolution/default-calendar '"435f6b86f964920b58adf0006dce4ce9bcc23cae"'
  # ID varies between between machines - above is for Fry, but for Crow it's "056e08cc9ac34d9e9ae5821f069ca2ebcc5be446"
  # current Gnome calendar doesn't seem to allow setting this - I've had to temporarily install Evolution,
  # then run `dconf watch /`, and do `edit, accounts, gmail, edit, mark as default calendar`
  # although I don't think that's how I previously got the ID for Fry a few months ago...
  # anyway, I've also non-declaratively set colour for Gnome "personal" calendar to black,
  # just in order to make sure I don't accidentally set events there when I couldn't work out how to change default
  # so maybe that's not important now
  # but I do probably want to work out how to set builtin calendar colours anyway,
  # as GMail and Cardiff City look a bit too close
  # in fact, while writing this, I've just non-declaratively set GMail to a lighter blue, on both machines
  environment.etc."xdg/ghc/ghci.conf".text = ''
    :seti -XGHC2021
    :seti -XBlockArguments
    :seti -XLambdaCase
    :seti -XOverloadedStrings

    :set -interactive-print Text.Pretty.Simple.pPrintForceColor

    -- TODO hmm, weird behaviour on Crow with `cabal repl` in `aoc-2025` repo
    -- I think maybe GHCI just doesn't handle non-ASCII characters well when counting movements
    -- EDIT all good now thanks to: https://wiki.haskell.org/GHCi_in_colour
    :set prompt-function \modules _lineno -> pure let c1 = "\x1b[1;38;2;69;58;98m\STX" ; c2 = "\x1b[1;38;2;94;80;134m\STX" ; c3 = "\x1b[1;38;2;143;78;139m\STX" ; ms = Data.List.unwords (Data.Maybe.mapMaybe (Data.List.uncons Control.Monad.>=> \(x, xs) -> Control.Monad.guard (x == '*') Data.Functor.$> xs) modules) in c3 <> ms <> (if Data.List.null ms then "" else " ") <> (c2 <> "λ" <> c1 <> ">") <> " \x1b[0m\STX"
    :set prompt-cont " | "

    :set +m
    :set +t

    -- TODO
    -- /etc should work, but GHC like so many others doesnt read XDG_CONFIG_DIRS
    -- https://downloads.haskell.org/~ghc/latest/docs/users_guide/ghci.html
    -- sudo ln -s /sync/tmp/ghci /etc/xdg/ghc/ghci.conf

    -- TODO
    -- do something about dependency unavailability
    -- prompt function only uses `base` and makes very few prelude assumptions
    -- but would be nice to delegate to `pretty-simple` binary when `pretty-simple` isn't available but `process` is
    -- and if neither, then maybe to at least avoid showing error
    -- there doesn't seem to be a decent way to do this in GHCI
    -- if we could get GHCI command result as actual Haskell string, we could e.g. use `:complete repl 1 "import Data.Text"`

    -- TODO
    -- still getting occasional cursor weirdness?
    -- hard to reproduce, so maybe just keep an eye on it

    -- TODO
    -- would like to add `:set +s`
    -- it's just a bit annoying because it seems to apply during load
    -- and we can't use `:seti +s` to avoid that

    -- TODO
    -- prompt colours look slightly pallide in VSCode for some reason
    -- especially since reversing order (needed for more visibility, esp. in Gnome Terminal)

    -- I don't really use GHCI enough to care a great deal about any of these later issues...
  '';

  environment.etc."xdg/opencode/opencode.jsonc".text = ''
    {
      "$schema": "https://opencode.ai/config.json",
      // re-enables OC LSP support
      // amazingly, this was recently disabled by default with little explanation
      // see e.g. https://github.com/anomalyco/opencode/issues/23417#issuecomment-4277503209.
      "lsp": true,
      "keybinds": {
        "messages_line_up": "shift+pageup,ctrl+alt+y",
        "messages_line_down": "shift+pagedown,ctrl+alt+e"
      },
      "permission": {
        // TODO what I actually want, really, is more fine-grained control over transient permissions
        // e.g. when OC requests access to a path, grant access to the parent's parent for the next 5 minutes/messages
        // this doesn't seem to be working anyway, but maybe I need to log out and in after fixing the symlink
        "external_directory": {
          "/nix/store/**": "allow"
        }
      },
      "mcp": {
        "local-demo": {
          "type": "remote",
          "url": "http://localhost:8000/mcp",
          "enabled": true
        }
      }
    }
  '';
  environment.etc."xdg/opencode/plugins/notification.js".text = ''
    // https://opencode.ai/docs/plugins/#send-notifications
    export const NotificationPlugin = async ({ project, client, $, directory, worktree }) => {
      return {
        event: async ({ event }) => {
          // Send notification on session completion
          if (event.type === "session.idle") {
            // await $`osascript -e 'display notification "Session completed!" with title "opencode"'`
            await $`${pkgs.lib.getExe pkgs.libnotify} -c notify-send -e OpenCode Done`
            // console.log(JSON.stringify(event))
          }
        },
      }
    }
  '';

  environment.etc."xdg/gtk-4.0/gtk.css".text = ''
    /* ooh, I've fucked up at some point and removed GTK dark preference (see Spotify and file picker)
    hopefully this goes away with a rebuild or at least once I've finished with my current working dir mess
    */

    /*
    ln -s /sync/tmp/gtk.css /home/gthomas/.config/gtk-4.0/gtk.css

    really, I want this to somehow apply only to tiled windows and not floating ones
    but now that I'm not using Forge, that distinction maybe doesn't really exist
    since I don't see this being solved any time soon, maybe just somehow commit to tiling and never float
    well I don't know if that's realistic - there'll always be temporary dialogs
    actually, maybe there is some CSS class that gets set? how does one debug this sort of thing? `:tiled`?
    maybe wait and see what my tiling workflow ends up looking like before spending time on this
    actually, in-app pop-ups (e.g. customise keyboard shortcuts) do remain curved, I guess because they're not windows
    oh hang on, native Gnome tiling already removes the border radius, so it's really an issue with the extension
    EDIT: hmm, not sure that last part is true
    similarly, setting to fullscreen with the extension leaves a visible gap at the top, possibly one pixel
    EDIT (19/01/2026, a few months after above): https://github.com/domferr/tilingshell/issues/463#issuecomment-3763034386
    */

    /* * {
      border-radius: 0;
    } */

    /*
    nope, this does nothing for panel...
    */
    window, panel, #panel {
      border-radius: 0;
    }

    /*
    was hoping to use something like this to hide the builtin pointless CSD title bars on apps like Element and Discord
    but it doesn't work - GTK just doesn't support much CSS, e.g. only `min-height` but not `height`
    we can get about half way there with negative margins and stuff, but I don't think there's a way
    https://docs.gtk.org/gtk3/css-properties.html
    .titlebar {
      height: 0;
      background-color: red;
    }
    */

    /* window { background-color: red; } */
    /* window:tiled { background-color: red; } */
    /* * { border-radius: 0; } */
  '';
  environment.etc."xdg/epiphany/user-stylesheet.css".text = ''
    /* dconf write /org/gnome/epiphany/web/enable-user-css true */
    /* /home/gthomas/.local/share/epiphany/user-stylesheet.css */
    /* this actually live-reloads which is pretty cool */

    /* move to somewhere in `XDG_DATA_DIRS` to configure via NixOS config?
    doesn't work - open upstream issue... */

    /*
      I'm thinking for now, for stuff that's very much a workaround like this and GTK border-radius,
      maybe it is fine to just use `system.activationScripts` and document why we're needing to do things a hacky way
      for the GTK the
    */

    /* body {
      transform: rotate(180deg);
      background-color: blue;
    } */

    svg:root {
      /* background for text files in Epiphany - better than white! */
      background-color: #1e1e1e;
    }
  '';
}
