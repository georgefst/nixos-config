# vibed, but does the job... we'll try removing this after the next major NixOS bump (26.11)

# PipeWire 1.6.5 (NixOS 26.05) introduced the lc3_a127 HFP codec (LC3 at
# 24kHz, aka "Apple 127"). This codec is selected by default for handsfree
# mode because it has the highest priority, but the SCO audio transport
# repeatedly fails with AirPods Pro - the mic works but there is no sound
# output. The log signature is:
#
#   spa.bluez5: Failure in Bluetooth audio transport ...
#   pw.node: (bluez_output...) running -> error
#
# There is no way to exclude a single codec from the default list. When
# bluez5.codecs is set in monitor.bluez.properties, only listed codecs are
# enabled (plus mandatory ones: sbc, cvsd, lc3). So we must list everything
# we want to keep.
#
# The full set of codecs that PipeWire can load is defined in:
#   https://gitlab.freedesktop.org/pipewire/pipewire/-/blob/master/spa/plugins/bluez5/codec-loader.c
#
# References:
#   - https://wiki.archlinux.org/title/Bluetooth_headset
#     (notes LC3/LE Audio as "still experimental" as of early 2025)
{ ... }: {
  services.pipewire.wireplumber.extraConfig."50-disable-hfp-lc3-a127" = {
    "monitor.bluez.properties" = {
      "bluez5.codecs" = [
        # A2DP codecs (music/media playback)
        "sbc"
        "sbc_xq"
        "aac"
        "aac_eld"
        "aptx"
        "aptx_hd"
        "aptx_ll"
        "aptx_ll_duplex"
        "ldac"
        "lc3plus_hr"
        "faststream"
        "faststream_duplex"
        "opus_g"
        "opus_05"
        "opus_05_51"
        "opus_05_71"
        "opus_05_duplex"
        "opus_05_pro"
        "mpeg"
        "g722"

        # HFP codecs (handsfree/mic)
        "cvsd" # narrowband 8kHz, the baseline
        "msbc" # wideband 16kHz, the reliable standard
        "lc3_swb" # super-wideband 32kHz
        # "lc3_a127" -- EXCLUDED: broken SCO output with AirPods Pro
      ];
    };
  };
}
