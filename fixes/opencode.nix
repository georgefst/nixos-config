# allow restarting LSP servers
# https://github.com/anomalyco/opencode/pull/6233
final: prev: {
  opencode = prev.opencode.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./opencode-lsp-restart.patch
    ];
  });
}
