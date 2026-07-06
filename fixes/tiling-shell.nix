# we can remove this once there's no longer anything interesting in `patches` field
final: prev: {
  gnomeExtensions = prev.gnomeExtensions // {
    tiling-shell = final.buildNpmPackage {
      pname = "gnome-shell-extension-tiling-shell";
      version = "17.3-patched-06-07-2026";
      src = final.fetchFromGitHub {
        owner = "domferr";
        repo = "tilingshell";
        rev = "de30eb72d5436424929e78b0be612114ba073a68";
        sha256 = "ZtlrMJkgYKKQIiZCJFKayAKeMmAZprjPNxX7siiOvks=";
      };
      patches = [
        # adds package-lock.json - needed for Nix
        (final.fetchpatch {
          url = "https://github.com/georgefst/tilingshell/commit/4c85456.patch";
          sha256 = "ZRtqexu2gKrfQWtlSRjyPaMVcCA6xxPGUd/f2Xh4cdQ=";
        })
        # https://github.com/domferr/tilingshell/pull/474
        (final.fetchpatch {
          url = "https://github.com/domferr/tilingshell/pull/474.patch";
          sha256 = "kmToAg35wsEPiQPijJq+hz72ZTniWLis95gQdqsGdsY=";
        })
        # https://github.com/domferr/tilingshell/pull/584
        (final.fetchpatch {
          url = "https://github.com/domferr/tilingshell/pull/584.patch";
          sha256 = "YdA0qtPv9AWJ3EdW1TFtOI6FQjBZsKVyMf0b9aVGe1M=";
        })
      ];
      nativeBuildInputs = [ final.glib ];
      npmDepsHash = "sha256-ctNiJ+Esf0TOuqbJBz53rQLqSkwn875woDrEl8rJo3A=";
      dontNpmInstall = true;
      npmFlags = [ "--legacy-peer-deps" ];
      installPhase = ''
        runHook preInstall
        mkdir -p $out/share/gnome-shell/extensions/tilingshell@ferrarodomenico.com
        cp -r dist/* $out/share/gnome-shell/extensions/tilingshell@ferrarodomenico.com/
        runHook postInstall
      '';
      passthru = {
        extensionUuid = "tilingshell@ferrarodomenico.com";
        extensionPortalSlug = "tiling-shell";
      };
    };
  };
}
