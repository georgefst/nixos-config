# Patch hide-cursor to use motion-event instead of position-invalidated
# This prevents the cursor from showing when another extension (e.g. mouse-follows-focus) warps it
final: prev: {
  gnomeExtensions = prev.gnomeExtensions // {
    hide-cursor = prev.gnomeExtensions.hide-cursor.overrideAttrs (oldAttrs: {
      patches = (oldAttrs.patches or []) ++ [
        ./hide-cursor.patch
      ];
    });
  };
}
