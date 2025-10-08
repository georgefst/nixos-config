#!/usr/bin/env bash

if realpath "$(which nixos-rebuild)" | grep -q "nixos-rebuild-ng"; then
    nixos-rebuild switch --flake .#$(hostname) --sudo
else
    nix shell nixpkgs#nixos-rebuild-ng -c nixos-rebuild-ng switch --flake .#$(hostname) --sudo
fi
