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
#
#
# ── possible future directions ───────────────────────────────────────────────────────────────────
#
# none of this is needed for it to work - it works now, over X11, with decent latency even on WiFi.
# recorded because the reasoning was expensive to reconstruct, not because it needs doing
#
# 1. LOWER LATENCY, BY GOING STRAIGHT TO A DRM PLANE. the console/KMS output (`CVideoDisplayDRM`)
#    hands the `YU12` dmabuf directly to a hardware overlay plane, so the display hardware does the
#    YUV->RGB and the scaling: no conversion pass and no compositing at all. `modetest -p` on Sol
#    shows all 56 planes (4 primary, 52 overlay) accept `YU12`, and KWin reports atomic modesetting,
#    so this should just work. the catch is DRM master: only one process can hold it, and KWin does,
#    so Steam Link can't run inside the Plasma session.
#    it does *not* require logging out, though - concurrent sessions on separate VTs are the normal
#    mechanism, and when Bigscreen's VT goes inactive logind pauses KWin's devices and it drops DRM
#    master. what's missing is a clean one-click launch: starting a second session means going via
#    the greeter, and while SDDM does implement `org.freedesktop.DisplayManager.Seat0.SwitchToGreeter`
#    so a Bigscreen entry could call it, you'd still pick the session by hand. worth it only if the
#    X11 path's conversion cost ever actually shows up as latency
#
# 2. WHAT NOT TO BOTHER WITH. a nested single-app wlroots compositor (`cage -- steamlink`) looks
#    appealing - it would keep everything inside Bigscreen with no X11, and wlroots advertises
#    external-only dmabuf formats where KWin doesn't, which is plausibly why upstream works on
#    Raspberry Pi OS (labwc) - but it's strictly worse than what we have: KWin can direct-scanout a
#    fullscreen XWayland surface, so the X11 path pays a conversion pass and probably no composite,
#    whereas cage pays a conversion pass *and* a composite. it also needs the nested compositor to
#    provide XWayland, since the bundled Qt 5.14 has no Wayland QPA plugin and the UI and the video
#    would otherwise land in different compositors. and see the FFmpeg note for why the Pi's FFmpeg
#    fork is not the answer either
#
# 3. RESOLUTION AND ASPECT RATIO. Remote Play captures a real output, so the stream takes its
#    geometry from the *host*: Fry is 2880x1920 at scale 2.0, so it renders ~5.5Mpx to send 1.75Mpx,
#    and Sol pillarboxes the 3:2 result into 16:9 (`Video rect: 1620x1080 at 150,0`). Steam's own
#    "change desktop resolution to match streaming client" drives a mode switch, which it can do on
#    Windows and under X11 but not through the Wayland screencast portal - so Wayland is *worse* here,
#    and this is a Steam limitation rather than a protocol one.
#    we can do the mode switch ourselves, at least on Gnome: Mutter ships `gdctl`, and Fry's panel has
#    a native 1920x1080 mode (`gdctl show --modes`), so setting eDP-1 to 1920x1080 at scale 1.0 before
#    streaming would match Sol exactly, drop the pillarboxing and stop Fry rendering pixels nobody
#    sees. `vkms` plus the portal's monitor picker is the fancier alternative; `gnome-remote-desktop`
#    is the better tool if the goal is ever general remote desktop rather than game streaming
#
# 4. STARTING THE HOST'S STEAM FROM SOL. the app has no pre-launch hook, so this would be a wrapper
#    around `steamlink` that starts Steam on the host, waits for it to answer discovery, then execs
#    the app - and it may as well do (3) at the same time, restoring on exit. Steam has to run inside
#    an existing graphical session (it needs an output to capture), so:
#
#      ssh fry 'systemd-run --user --collect --unit=steam-remote-play steam'
#
#    `systemd-run --user` hands it to the user manager, whose environment already carries `DISPLAY`
#    and `WAYLAND_DISPLAY`, and the transient unit outlives the SSH connection. Sol's key is already
#    authorised everywhere (`modules/universal.nix`), but `programs.ssh.knownHosts` currently only
#    exists in `modules/desktop.nix`, so Sol would need it too - the keys are already in
#    `nix/devices.nix`, whose TODO anticipates exactly this.
#    the open question is the portal: if `xdg-desktop-portal-gnome` re-prompts for screen capture
#    every launch, none of this can be unattended. test by starting Steam, sharing, quitting and
#    starting again - if it only asks once, it's persisting a restore token and this is viable
#
# 5. WIRE SOL UP. latency is already decent, but Sol is on WiFi (`wld0`) with its gigabit `end0`
#    sitting `DOWN`, and Remote Play cares far more about jitter than about bandwidth - a stall shows
#    up as a dropped frame immediately, and the host's encoder reacts by dropping the bitrate. this is
#    the cheapest available improvement by a wide margin, and worth doing before any of the more
#    interesting ideas above, if only so that they're measured against a stable baseline
#
# 6. WHETHER A PI IS THE RIGHT BOX AT ALL. everything painful here traces back to one fact: this
#    machine has to decode H.264 in software, because the Pi 5 has no H.264 block and Valve's client
#    speaks nothing else. that's survivable - reportedly the Pi 5 in software beats the Pi 4's decoder
#    outright - but it spends four cores' worth of power on something fixed-function silicon does for
#    a fraction of it, and it's what forces the X11 detour, since the `yuv420p` it produces is the one
#    format the compositor won't take.
#    a small x86 box would dissolve the whole problem rather than work around it: hardware H.264 *and*
#    HEVC decode through VAAPI, and no need for this package at all, since `programs.steam` gives you
#    the real client with Big Picture and Remote Play built in - the same thing Fry is already running
#    as the host. no blob to pin, no soname to match, no update ritual, no format negotiation.
#    the Pi's case here used to be price, and that's much weaker now: by the time a Pi 5 has PSU,
#    case, cooler and storage it's within reach of an N100-class mini PC that draws similar idle power
#    and is several times quicker. worth remembering before sinking more effort into (1)-(4) - most of
#    that work exists only to compensate for the hardware
#
# 7. `Hardware: Unknown`, logged at startup, is read from the `Hardware` field of `/proc/cpuinfo`,
#    which arm64 never emits - it's an arm32 field, and Sol reports `Model`/`Revision` instead. so no
#    64-bit Pi can satisfy it. whether it feeds anything beyond the telemetry blob is unchecked, but
#    it can't matter here: we know from Fry's log which path was taken, and there is no better one
#    available to choose. to check anyway, grep the binary for `cpuinfo` near a `BCM2*` model table:
#
#     tr -c '[:print:]' '\n' < /run/current-system/sw/share/steamlink/bin/shell | grep -E '.{6,}' \
#       | grep -niE 'cpuinfo|BCM2|Raspberry Pi [0-9]'
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
    # this is vanilla Nixpkgs FFmpeg, where a Debian install would get Raspberry Pi's fork, so video
    # is decoded in *software*. `nixos-raspberrypi` does package the fork we'd otherwise want, as
    # `pkgs/ffmpeg_7-rpi.nix` (jc-kynesim/rpi-ffmpeg 7.1.2, `--enable-v4l2-request --enable-sand`,
    # and conveniently the same sonames), and we could pass it here alone without taking their
    # overlay over the whole graphical stack - but it would buy us precisely nothing:
    #   - the Pi 5 dropped the H.264 decode block entirely (`/dev/v4l/by-path` has only
    #     `1000800000.codec`, i.e. rpivid, and no `/dev/video1{0,1,2}` `bcm2835-codec` nodes)
    #   - rpivid is HEVC-only, and the app has no HEVC decoder: its RTTI lists exactly
    #     `CStreamDecoder{Video,H264,H264AVCodec,H264Standalone,Opus,RawAudio,RawVideo}`, and the
    #     H.264 NAL handling in `streamdecoderh264.cpp` isn't secretly generic. the `--enable-hevc`,
    #     `--enable-av1` and `k_EStreamVideoCodecHEVC` strings are from Steam's shared cross-platform
    #     codebase - the same flag table also has `--d3d9`/`--d3d11` - so they prove nothing. Fry's
    #     host log agrees: `Allowed Codecs: 4` (`k_EStreamVideoCodecH264`), then
    #     `Created encoder VAAPI for codec 4` and `Client video decoder set to Raspberry Pi software
    #     decoding on ...`
    #   - and it wouldn't fix the *display* blocker either (see the wrapper below): the software
    #     H.264 decoder emits `yuv420p` whichever FFmpeg it's linked against; `--enable-sand` only
    #     changes what the fork's own hardware decoders produce
    # so: software H.264 forever on this box. note that a Pi *4* would sidestep all of this - its
    # `bcm2835-codec` is a stateful V4L2 M2M device, which stock Nixpkgs FFmpeg already drives
    # (`--enable-v4l2-m2m`, and `h264_v4l2m2m` is present), and it decodes to NV12, which KWin *does*
    # advertise - so it would work under Bigscreen with no extra packaging, no X11 and no copies.
    # that's an elegance win rather than a speed one, though: the Pi 5's cores are quick enough that
    # software H.264 is reportedly *faster* than the Pi 4's decode block, just far less efficient -
    # four cores burning watts to do what fixed-function silicon does for milliwatts. see (6) in the
    # notes at the top of this file
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
    # the Qt platform selection is upstream's: `xcb` under a display server (for us, XWayland - see
    # the note on the X11 dependencies above), otherwise the framebuffer on a bare console.
    #
    # `SDL_VIDEO_DRIVER=x11` is ours, and it is what makes video appear at all on a Pi 5. the app has
    # three display outputs, and picks one according to the video driver SDL initialises - its RTTI
    # names them `CVideoDisplayWayland`, `CVideoDisplayEGL` (the X11 one) and `CVideoDisplayDRM`
    # (console/KMS). the Wayland one presents frames as dmabufs via `zwp_linux_dmabuf_v1` (the
    # embedded copy of jc-kynesim's `hello_wayland`/`drmu`, from `testffmpeg_rpi`) and drops any frame
    # whose format the compositor doesn't advertise. that can never work here:
    #   - the Pi 5 has no H.264 decode block at all (only HEVC, via rpivid at
    #     /dev/v4l/by-path/platform-1000800000.codec-video-index0), and the app has no HEVC decoder
    #     anyway - only `CStreamDecoderH264{,AVCodec,Standalone}` - so it always software-decodes
    #   - software decode yields `yuv420p`, i.e. DRM format `YU12` (3-plane planar)
    #   - KWin on V3D advertises NV12, P010, XYUV and the RGB formats, but *not* YU12 (checked with
    #     `wayland-info`, which is in Nixpkgs as `wayland-utils`)
    # so every frame is dropped with `No support for format YU12 mod 0`: perfect audio, black screen.
    # forcing SDL to X11 selects `CVideoDisplayEGL` instead, which takes `yuv420p` without complaint.
    # the app itself notes X11 is slower ("performance is better under Wayland or the console"), but
    # slower beats invisible. only set when there's actually an X display, so a bare console still
    # gets SDL's `kmsdrm` and the direct-to-KMS output, and always overridable for comparing paths.
    #
    # NB. two things here are inference rather than measurement, and both are recorded as such
    # deliberately, since a wrong "why" is worse than an admitted gap:
    #  1. *why* KWin doesn't advertise YU12. "V3D can't import 3-plane planar" is one explanation and
    #     may well have been observed rather than assumed, but that isn't clear, and the competing one
    #     fits the evidence just as well: Mesa reports YU12 as *external-only* (samplable only as
    #     `GL_TEXTURE_EXTERNAL_OES`) and KWin filters external-only formats out of its advertised set.
    #     KWin's list containing R8/GR88/R16/RG16 - exactly the per-plane formats Mesa uses for YUV
    #     lowering - is mildly suggestive of the latter
    #  2. *how* `CVideoDisplayEGL` gets away with YU12: either it imports the dmabuf via EGL as an
    #     external texture (near zero-copy), or it falls back to `SDL_UpdateYUVTexture` (a real CPU
    #     copy, ~2.6MB/frame, ~160MB/s at 1080p60). the binary imports both, and the log is identical
    #     either way, so this is open
    # one command settles both at once - while a stream is running, on Sol:
    #   p=$(pgrep -x shell); ls -l /proc/$p/fd | grep -Ec 'dma_heap|dmabuf'; top -b -n1 -p $p | tail -2
    # dmabuf fds open => EGL import, so (2) is zero-copy and (1) is the external-only explanation
    # (V3D *can* import it, KWin just won't say so). none => the CPU upload, and (1) stands as written
    makeWrapper $out/share/steamlink/bin/shell $out/bin/steamlink \
      --prefix PATH : $out/share/steamlink/bin \
      --prefix LD_LIBRARY_PATH : $out/share/steamlink/lib:$qtDir/lib \
      --set QTDIR $qtDir \
      --set QT_PLUGIN_PATH $qtDir/plugins \
      --run 'export QT_QPA_PLATFORM="''${QT_QPA_PLATFORM:-''${DISPLAY:+xcb}}"' \
      --run 'export QT_QPA_PLATFORM="''${QT_QPA_PLATFORM:-linuxfb}"' \
      --run 'export SDL_VIDEO_DRIVER="''${SDL_VIDEO_DRIVER:-''${DISPLAY:+x11}}"' \
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
