# HiFiBerry's DSP toolkit - <https://github.com/hifiberry/hifiberry-dsp>.
#
# This is for Sol's HiFiBerry DAC+ DSP board, whose ADAU1451 DSP is the only thing that can see
# (and hence attenuate) the Toslink input - see the long comment in `modules/sol.nix`.
#
# We do *not* run any of this at runtime: the `sigmatcpserver` daemon exists mainly to speak the
# SigmaStudio TCP protocol, and the volume control it offers is a polling shim that fakes an ALSA
# control and mirrors it onto the DSP once a second. The Haskell script writes the DSP register
# directly instead. What this package *is* for is the things that would be miserable to reimplement:
# identifying which DSP profile is flashed in the board's EEPROM, reading its register map, and
# installing a new profile. See "Discovery procedure" below.
#
# Upstream is pure Python - no C library, no shared object, nothing to bind to. The entire hardware
# layer is `hifiberrydsp/hardware/spi.py`, 112 lines wrapping the `spidev` module (itself a thin
# wrapper over `ioctl(SPI_IOC_MESSAGE)` on `/dev/spidev0.0`). The DSP wire protocol is a single
# full-duplex SPI transfer of `[rw, addr_hi, addr_lo, data...]`, `rw` being 0 to write and 1 to read.
# The other ~10k lines are the TCP server, XML profile parsing, biquad/FIR filter math, a Flask REST
# API and a filter store.
#
# ## Discovery procedure
#
# Everything the DSP exposes lives at a *profile-specific* memory address: the volume control, the
# per-input volume limits, the mute registers. The addresses come from a `<beometa>` block in the
# SigmaStudio XML profile, and differ completely between profiles (`dacdsp-default.xml` puts
# `volumeControlRegister` at 4573, `dacdsp-v12-1.xml` at 47). The board self-boots its profile from
# an onboard EEPROM, so the authoritative answer is whatever was flashed at the factory.
#
# To find out, on Sol (this needs `dtparam=spi=on`, set in `flake.nix`):
#
#     # 1. confirm SPI reaches the DSP at all. `0xf000` is the PLL feedback divider, which must be
#     #    non-zero on a running part. Prefer this over `Adau145x.detect_dsp()`, which writes the
#     #    reset register. The ADAU145x powers up in I2C mode and only latches into SPI after three
#     #    chip-select transitions, so discard the first couple of reads.
#     dsptoolkit read-hex 0xf000
#
#     # 2. checksum the running program, and match it against the profiles in `share/` below
#     sigmatcpserver --enable-rest --disable-tcp --localhost &
#     curl -s localhost:13141/checksum
#     curl -s localhost:13141/profiles/metadata | jq
#
# `/checksum` reads program memory, which requires halting the DSP core, so expect a momentary
# audio dropout. Match the MD5 against the `checksum` metadata of the bundled profiles:
#
#     dacdsp-default.xml   16EA9EE2C6A296BDBF4C2C3A55246729   volumeControlRegister = 4573
#     dacdsp-v12-1.xml     CB71C7D437125A4CE066798726B1D25D   volumeControlRegister = 47
#
# The results of running this are recorded in `modules/sol.nix`, which is where the addresses the
# Haskell script uses are configured.
#
# Note that the REST API has no volume endpoint at all, despite upstream marking the `dsptoolkit`
# CLI (which does) as deprecated in its favour. Volume over REST means a raw write to `/memory`.
{ lib, python3Packages, alsa-utils, src }:

python3Packages.buildPythonApplication rec {
  pname = "hifiberry-dsp";
  version = "1.3.13"; # `src/hifiberrydsp/__init__.py`
  format = "setuptools";

  inherit src;
  # `setup.py` lives in `src/`, not at the repo root ("source" being what a flake input unpacks as)
  sourceRoot = "source/src";

  propagatedBuildInputs = with python3Packages; [
    xmltodict
    spidev
    pyalsaaudio
    requests
    flask
    waitress
  ];

  # There is no test suite to speak of, and what exists wants real hardware.
  doCheck = false;

  postPatch = ''
    # `--alsa` mode synthesises its dummy mixer control by writing a state file and shelling out to
    # `alsactl restore`. Without this the call silently fails and no control ever appears.
    substituteInPlace hifiberrydsp/alsa/alsasync.py \
      --replace-fail '/usr/sbin/alsactl' '${lib.getExe' alsa-utils "alsactl"}'

    # Read-only data directory of known DSP profiles, used by the `/profiles*` REST endpoints to
    # turn a program checksum into a register map. Upstream ships it via the Debian package; here it
    # is installed from `sample_files/xml` (see `postInstall`).
    substituteInPlace hifiberrydsp/api/restapi.py hifiberrydsp/api/settings_store.py hifiberrydsp/server/sigmatcp.py \
      --replace-fail '/usr/share/hifiberry/dspprofiles' "$out/share/hifiberry/dspprofiles"
  '';

  # Deliberately *not* patched: `/var/lib/hifiberry` (genuine mutable state, only used when running
  # as root, and a perfectly good location on NixOS), `/etc/sigmatcp.conf` (optional config file),
  # and `dsptoolkit.py`'s `/etc/dspprogram.xml` and `/etc/dspparameter.dat` (dead constants).

  postInstall = ''
    mkdir -p $out/share/hifiberry/dspprofiles
    cp ${src}/sample_files/xml/*.xml $out/share/hifiberry/dspprofiles/
  '';

  meta = {
    description = "Tools to configure HiFiBerry DSP boards and program them from SigmaStudio";
    homepage = "https://github.com/hifiberry/hifiberry-dsp";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
