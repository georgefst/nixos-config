# Valve's Steam Link app for the Raspberry Pi
#
# there is no `steamlink` in Nixpkgs (the only Steam Link packaging anywhere is the Flathub one,
# which is `only-arches: [x86_64]`), and `programs.steam` is no use to us -
# that's the full Steam *client*, whose binaries are x86 only, hence the
# `hardware.graphics.enable32Bit is only supported on an x86_64 system` assertion on aarch64
#
# Valve do however still ship current aarch64 builds for the Pi, at the URL that the Raspberry Pi OS
# `steamlink` package's bootstrap script (`steamlink_1.0.16_all.deb`, `Architecture: all`) resolves at runtime:
#   https://media.steampowered.com/steamlink/rpi/$VERSION_CODENAME/$(dpkg --print-architecture)/public_build.txt
# we pin the tarball rather than letting it self-update, so `steamlinkdeps` (its apt-install-the-missing-libs
# step) and the whole `/usr/bin/steamlink` download-and-verify wrapper are irrelevant to us -
# we do still take the deb, but only for the icons, which aren't in the tarball.
# `trixie` is the newest codename Valve build for - the codename only picks which Debian release the
# blobs were linked against, and everything they need is satisfied below
#
# we also don't use the bundled `steamlink.sh` launcher, which is a poor fit for NixOS:
# it shells out to `dpkg --print-architecture`, `sudo cp`s udev rules into `/lib/udev/rules.d` and
# `usermod`s the calling user into `input`/`plugdev` on first run (`sol.nix` does that declaratively),
# and points `TMPDIR` at its own - here read-only - install directory.
# what's left of it is the env var block at the bottom, which is what we reproduce
{ lib
, stdenv
, fetchurl
, autoPatchelfHook
, copyDesktopItems
, dpkg
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
, xorg
, zlib
, zstd
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "steamlink";
  version = "1.3.32.316";

  src = fetchurl {
    url = "https://media.steampowered.com/steamlink/rpi/trixie/arm64/steamlink-rpi-trixie-arm64-${finalAttrs.version}.tar.gz";
    hash = "sha256-Mvc38BZ3J7/3OaW3k7hyIrYD7A2ng3mRpJPnqB53u2I=";
  };

  # icons only - the version is unrelated to (and much older than) the app's, since this is
  # just the bootstrapper package, and `Architecture: all`
  deb = fetchurl {
    url = "https://archive.raspberrypi.org/debian/pool/main/s/steamlink/steamlink_1.0.16_all.deb";
    hash = "sha256-N+ZWD6zMwT3IrjdG4FbXIBQRe/WRjLuA/HxEcoSK1QU=";
  };

  nativeBuildInputs = [ autoPatchelfHook copyDesktopItems dpkg makeWrapper ];

  # `DT_NEEDED` of the bundled `shell`/Qt/SDL blobs, plus the backends the bundled SDL 3 advertises
  # in its `.note.dlopen` (which `autoPatchelfHook` also resolves) - listed explicitly rather than
  # left to fall out of FFmpeg's closure, which happens to cover most of them
  buildInputs = [
    (lib.getLib stdenv.cc.cc) # libstdc++, libgcc_s
    alsa-lib
    dbus
    double-conversion
    ffmpeg_7 # `shell` wants libavcodec.so.61/libavutil.so.59, i.e. FFmpeg 7, not the default 8
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
    libxkbcommon
    md4c
    mtdev
    pipewire
    vulkan-loader
    wayland # libwayland-client, libwayland-egl - `shell` links these directly, for video output
    zlib
    zstd
  ] ++ (with xorg; [
    libICE
    libSM
    libX11
    libXcursor
    libXext
    libXfixes
    libXi
    libXrandr
    libXScrnSaver
    libXtst
    libxcb
    xcbutilimage
    xcbutilkeysyms
    xcbutilrenderutil
    xcbutilwm # libxcb-icccm
  ]);

  # more of the bundled SDL 3's optional `.note.dlopen` backends, none of which are packaged here
  # (and none of which matter: no VR, no GLES 1.x, no sndio, and Steamworks only exists inside games)
  autoPatchelfIgnoreMissingDeps = [
    "libGLES_CM.so.1"
    "libopenxr_loader.so.1"
    "libsndio.so.7"
    "libsteam_api.so"
  ];

  qtDir = "Qt-5.14.1";

  desktopItems = [
    (makeDesktopItem {
      name = "steamlink";
      desktopName = "Steam Link";
      comment = "Application for managing and playing games on Steam";
      exec = "steamlink %u";
      icon = "steamlink";
      categories = [ "Game" ];
      mimeTypes = [ "x-scheme-handler/steamlink" ];
    })
  ];

  installPhase = ''
    runHook preInstall

    # upstream ships everything 0700/0600
    chmod -R u+w,go+rX .

    mkdir -p $out/share/steamlink
    cp -r bin lib ${finalAttrs.qtDir} version.txt $out/share/steamlink/
    install -Dm644 -t $out/share/doc/steamlink \
      LICENSE.txt ThirdPartyLegalNotices.css ThirdPartyLegalNotices.html

    dpkg-deb -x ${finalAttrs.deb} deb
    cp -r deb/usr/share/icons $out/share/

    # `libsteamwebrtc.so` isn't in anything's `DT_NEEDED`, so it's dlopened by bare soname and
    # `autoPatchelfHook` can't reach it via RUNPATH - hence `LD_LIBRARY_PATH`, as upstream does
    # (`QT_PLUGIN_PATH` likewise, since a Qt 5.14 with no `qt.conf` won't find plugins under `$out`).
    # `bin` holds helper scripts and the `vhusbdarmslpi{4,5}` USB-sharing daemons, invoked by name.
    # the bundled Qt is 5.14 and has no Wayland QPA plugin, so under a Wayland session the UI goes
    # through XWayland (`xcb`); on a bare console it falls back to the framebuffer, as upstream picks
    makeWrapper $out/share/steamlink/bin/shell $out/bin/steamlink \
      --prefix PATH : $out/share/steamlink/bin \
      --prefix LD_LIBRARY_PATH : $out/share/steamlink/lib:$out/share/steamlink/${finalAttrs.qtDir}/lib \
      --set QTDIR $out/share/steamlink/${finalAttrs.qtDir} \
      --set QT_PLUGIN_PATH $out/share/steamlink/${finalAttrs.qtDir}/plugins \
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
