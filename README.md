Build system configuration:

`nix shell nixpkgs/22.11#nixos-rebuild -c nixos-rebuild --flake .#clark build`

Build boot image:

`nix build .#images.clark`

## Relevant documentation
- https://nixos.org/manual/nixos/stable/options.html
- https://nixos.wiki/wiki/NixOS_on_ARM
- https://nixos.wiki/wiki/NixOS_on_ARM/Raspberry_Pi_3
