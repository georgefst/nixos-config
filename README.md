Build system configuration:

`nix shell nixpkgs/23.11#nixos-rebuild -c nixos-rebuild --flake .#clark build` (or `nix build .#configs.clark`, or `nix build`)

Build boot image:

`nix build .#images.clark`

Build Haskell program:

`nix build .#haskell.clark`

## Relevant documentation
- https://nixos.org/manual/nixos/stable/options.html
- https://nixos.wiki/wiki/NixOS_on_ARM
- https://nixos.wiki/wiki/NixOS_on_ARM/Raspberry_Pi_3
