# Valve's Steam Link app for the Raspberry Pi
#
# there is no `steamlink` in Nixpkgs (the only Steam Link packaging anywhere is the Flathub one,
# which is `only-arches: [x86_64]`), and `programs.steam` is no use to us -
# that's the full Steam *client*, whose binaries are x86 only, hence the
# `hardware.graphics.enable32Bit is only supported on an x86_64 system` assertion on aarch64
#
# Valve do however still ship current aarch64 builds for the Pi, at the URL that the Raspberry Pi OS
# `steamlink` package's bootstrap script (`steamlink_1.0.16_all.deb`, `Architecture: all`) resolves at
# runtime. we pin the tarball rather than letting it self-update, so `steamlinkdeps` (its
# apt-install-the-missing-libs step) and the whole `/usr/bin/steamlink` download-and-verify wrapper
# are irrelevant to us. the flipside is that we don't get the updates a Debian install would - if
# Valve ever make a server-side change that requires a newer client, Steam Link will simply stop
# working, and the fix is to bump the pin by hand. see the update notes below
#
# we also don't use the bundled `steamlink.sh` launcher, which is a poor fit for NixOS:
# it shells out to `dpkg --print-architecture`, `sudo cp`s udev rules into `/lib/udev/rules.d` and
# `usermod`s the calling user into `input`/`plugdev` on first run (`sol.nix` does that declaratively
# via `hardware.steam-hardware`), and points `TMPDIR` at its own - here read-only - install
# directory. what's left of it is the env var block at the bottom, which is what we reproduce
#
#
# ── updating ─────────────────────────────────────────────────────────────────────────────────────
#
# Valve publish no "latest" tarball URL and no directory index (`.../trixie/arm64/` is 403), so the
# current release has to be read out of a text file:
#
#     curl -s https://media.steampowered.com/steamlink/rpi/trixie/arm64/public_build.txt
#
# which prints the full URL of the current build. `trixie` is the newest Debian codename Valve build
# for - the codename only picks which Debian release the blobs were linked against, and everything
# they need is satisfied from Nixpkgs below. to check whether a newer one has appeared:
#
#     for c in bookworm trixie forky; do printf '%s: ' $c
#       curl -s https://media.steampowered.com/steamlink/rpi/$c/arm64/public_build.txt; done
#
# old builds are not deleted (1.3.24.301 from Feb 2026, and bookworm's 1.3.16.287 from Feb 2025,
# both still resolve), so an existing pin should keep working indefinitely.
#
# to bump: put the new version below, then
#
#     nix store prefetch-file --json <url> | jq -r .hash
#
# or just change `version`, build, and paste the `got:` hash out of the mismatch error.
#
# before trusting a bump, unpack the candidate next to what we're currently built against:
#
#     old=$(nix build --no-link --print-out-paths .#packages.aarch64-linux.steamlink)
#     mkdir new && curl -sL <url> | tar xz --strip-components=1 -C new
#
# and check the three things that can actually break:
#
# 1. THE LAUNCHER. we reproduce upstream's `steamlink.sh` env var block in the wrapper below, and a
#    change there builds perfectly cleanly - it only shows up as the app misbehaving at runtime.
#    `launcherHash` guards against that, so the build will stop and send you here. diff it, make
#    sure the wrapper still sets everything the new script does, then update the hash:
#
#     diff -u $old/share/doc/steamlink/steamlink.sh new/steamlink.sh
#     grep -nE 'export |QT_QPA_PLATFORM|LD_LIBRARY_PATH|QT_PLUGIN_PATH|QTDIR' new/steamlink.sh
#
# 2. THE BUNDLED QT. resolved by glob below, so a 5.14.1 -> 5.15.x move needs nothing, but a jump to
#    Qt 6 would change both the plugin layout and the platform selection at the bottom of this file
#    (and might finally bring a Wayland QPA plugin - see the note on the X11 dependencies):
#
#     ls -d new/Qt-*; ls new/Qt-*/plugins/platforms/
#
# 3. THE LIBRARIES. `autoPatchelfHook` fails loudly and precisely on these, so this is only a
#    preview - but it saves a build cycle:
#
#     diff -u $old/share/doc/steamlink/steamlinkdeps.txt new/steamlinkdeps.txt
#     patchelf --print-needed new/bin/shell new/lib/*.so* new/Qt-*/lib/*.so* | sort -u
#     readelf -p .note.dlopen new/lib/libSDL3.so.0
#
#    note that every entry in SDL's `.note.dlopen` is `"priority":"suggested"`, but
#    `autoPatchelfHook` ignores priority and hard-fails on any it can't resolve - hence both the
#    explicit backend libraries in `buildInputs` and `autoPatchelfIgnoreMissingDeps`
# ─────────────────────────────────────────────────────────────────────────────────────────────────
{ lib
, stdenv
, fetchurl
, autoPatchelfHook
, copyDesktopItems
, makeDesktopItem
, makeWrapper
, alsa-lib
, dbus
, double-conversion
, ffmpeg_7
, fontconfig
, freetype
, glib
, harfbuzz
, krb5
, libdecor
, libdrm
, libepoxy
, libgbm
, libglvnd
, libinput
, libjpeg_turbo
, libpng
, libpulseaudio
, libusb1
, libxkbcommon
, md4c
, mtdev
, pipewire
, vulkan-loader
, wayland
, zlib
, zstd
, libice
, libsm
, libx11
, libxcb
, libxcb-image
, libxcb-keysyms
, libxcb-render-util
, libxcb-wm
, libxcursor
, libxext
, libxfixes
, libxi
, libxrandr
, libxscrnsaver
, libxtst
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "steamlink";
  version = "1.3.32.316";

  src = fetchurl {
    url = "https://media.steampowered.com/steamlink/rpi/trixie/arm64/steamlink-rpi-trixie-arm64-${finalAttrs.version}.tar.gz";
    hash = "sha256-Mvc38BZ3J7/3OaW3k7hyIrYD7A2ng3mRpJPnqB53u2I=";
  };

  # tripwire on the launcher we deliberately don't run - see (1) in the update notes above
  launcherHash = "3c970a4c4e54e41aa1abc641af1f4dfcb875529fd387e7492a563a3a5a5865df";

  nativeBuildInputs = [ autoPatchelfHook copyDesktopItems makeWrapper ];

  # `DT_NEEDED` of the bundled `shell`/Qt/SDL blobs, plus the backends the bundled SDL 3 advertises
  # in its `.note.dlopen` (which `autoPatchelfHook` also resolves) - listed explicitly rather than
  # left to fall out of FFmpeg's closure, which happens to cover most of them
  buildInputs = [
    (lib.getLib stdenv.cc.cc) # libstdc++, libgcc_s
    alsa-lib
    dbus
    double-conversion
    # `shell` wants libavcodec.so.61/libavutil.so.59, i.e. FFmpeg 7, not the default 8.
    # note this is vanilla Nixpkgs FFmpeg, where a Debian install would get Raspberry Pi's fork,
    # so video is decoded in *software*. that costs us nothing on a Pi 5 for the H.264 that Remote
    # Play defaults to, because the Pi 5 dropped the H.264 decode block entirely (`/dev/v4l/by-path`
    # has only `1000800000.codec`, i.e. rpivid, and no `/dev/video1{0,1,2}` `bcm2835-codec` nodes).
    # rpivid is HEVC-only and driven through the V4L2 request API, whose `hevc_v4l2request` decoder
    # exists only in the Pi's fork - vanilla FFmpeg has no `--enable-v4l2-request` - so enabling HEVC
    # (`steamlink --enable-hevc`) would not buy hardware decode either. packaging the fork is the
    # only route to it, and worth it only if software 1080p60 turns out not to keep up
    ffmpeg_7
    fontconfig
    freetype
    glib
    harfbuzz
    krb5
    libdecor
    libdrm
    libepoxy
    libgbm
    libglvnd # libEGL, libGLESv2
    libinput
    libjpeg_turbo
    libpng
    libpulseaudio
    libusb1
    libxkbcommon # also libxkbcommon-x11
    md4c
    mtdev
    pipewire
    vulkan-loader
    wayland # libwayland-client, libwayland-egl - `shell` links these directly, for video output
    zlib
    zstd
  ] ++ [
    # X11, which is load-bearing *because* Bigscreen is Wayland, not left over from anything:
    # the bundled Qt 5.14 has no Wayland QPA plugin (`Qt-*/plugins/platforms/` holds only
    # `libqxcb.so`, `libqeglfs.so` and `libqlinuxfb.so`), so the UI necessarily runs under XWayland,
    # which is what drags in `libQt5XcbQpa` -> libX11/libxcb*/libICE/libSM. SDL likewise selects its
    # x11 backend rather than its wayland one, because `DISPLAY` is set - hence the rest
    libice
    libsm
    libx11
    libxcb
    libxcb-image
    libxcb-keysyms
    libxcb-render-util
    libxcb-wm
    libxcursor
    libxext
    libxfixes
    libxi
    libxrandr
    libxscrnsaver
    libxtst
  ];

  # more of the bundled SDL 3's optional `.note.dlopen` backends, none of which are packaged here
  # (and none of which matter: no VR, no GLES 1.x, no sndio, and Steamworks only exists inside games)
  autoPatchelfIgnoreMissingDeps = [
    "libGLES_CM.so.1"
    "libopenxr_loader.so.1"
    "libsndio.so.7"
    "libsteam_api.so"
  ];

  desktopItems = [
    (makeDesktopItem {
      name = "steamlink";
      desktopName = "Steam Link";
      comment = "Application for managing and playing games on Steam";
      exec = "steamlink %u";
      icon = ../assets/steamlink.png; # upstream only ship icons in the deb, so we vendor the 256px one
      categories = [ "Game" ];
      mimeTypes = [ "x-scheme-handler/steamlink" ];
    })
  ];

  installPhase = ''
    runHook preInstall

    got=$(sha256sum steamlink.sh | cut -d' ' -f1)
    if [[ "$got" != "${finalAttrs.launcherHash}" ]]; then
      echo "upstream's steamlink.sh has changed:"
      echo "  expected ${finalAttrs.launcherHash}"
      echo "  got      $got"
      echo "the wrapper in this derivation reproduces its env var block, so a change there would"
      echo "otherwise build cleanly and only surface as the app misbehaving at runtime."
      echo "see (1) in the update notes at the top of nix/steamlink.nix, then update launcherHash."
      exit 1
    fi

    mkdir -p $out/share/steamlink
    cp -r bin lib Qt-* version.txt $out/share/steamlink/
    # `autoPatchelfHook` rewrites the ELFs in place, and everything we just copied came out of a
    # read-only store path (the fixup phase makes it read-only again afterwards)
    chmod -R u+w $out/share/steamlink

    # `steamlink.sh` and `steamlinkdeps.txt` aren't used, but keeping them makes the diffs in the
    # update notes above possible against the previous build rather than a hand-kept copy
    install -Dm644 -t $out/share/doc/steamlink \
      LICENSE.txt ThirdPartyLegalNotices.css ThirdPartyLegalNotices.html \
      README.txt steamlink.sh steamlinkdeps.txt

    qtDirs=($out/share/steamlink/Qt-*)
    if [[ ''${#qtDirs[@]} != 1 ]]; then
      echo "expected exactly one bundled Qt, got: ''${qtDirs[*]}"
      echo "see (2) in the update notes at the top of nix/steamlink.nix."
      exit 1
    fi
    qtDir=''${qtDirs[0]}

    # `libsteamwebrtc.so` isn't in anything's `DT_NEEDED`, so it's dlopened by bare soname and
    # `autoPatchelfHook` can't reach it via RUNPATH - hence `LD_LIBRARY_PATH`, as upstream does
    # (`QT_PLUGIN_PATH` likewise, since a Qt 5.14 with no `qt.conf` won't find plugins under `$out`).
    # `bin` holds helper scripts and the `vhusbdarmslpi{4,5}` USB-sharing daemons, invoked by name.
    # the platform selection is upstream's: `xcb` under a display server (for us, XWayland - see the
    # note on the X11 dependencies above), otherwise the framebuffer on a bare console
    makeWrapper $out/share/steamlink/bin/shell $out/bin/steamlink \
      --prefix PATH : $out/share/steamlink/bin \
      --prefix LD_LIBRARY_PATH : $out/share/steamlink/lib:$qtDir/lib \
      --set QTDIR $qtDir \
      --set QT_PLUGIN_PATH $qtDir/plugins \
      --run 'export QT_QPA_PLATFORM="''${QT_QPA_PLATFORM:-''${DISPLAY:+xcb}}"' \
      --run 'export QT_QPA_PLATFORM="''${QT_QPA_PLATFORM:-linuxfb}"' \
      --run 'export SDL_GAMECONTROLLERCONFIG_FILE="''${XDG_DATA_HOME:-$HOME/.local/share}/Valve Corporation/SteamLink/controller_map.txt"'

    runHook postInstall
  '';

  meta = {
    description = "Stream games from a computer running Steam (Raspberry Pi build)";
    homepage = "https://store.steampowered.com/steamlink/about";
    license = lib.licenses.unfree;
    mainProgram = "steamlink";
    platforms = [ "aarch64-linux" ];
  };
})
