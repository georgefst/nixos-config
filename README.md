Build system configuration:

`nix shell nixpkgs/22.11#nixos-rebuild -c nixos-rebuild --flake .#clark build`

Build boot image:

`nix build .#images.clark`
