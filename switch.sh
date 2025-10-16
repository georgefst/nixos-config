#!/usr/bin/env bash

if [ -n "$1" ]
then
    REV="?rev=$(git rev-parse $1)"
fi

if realpath "$(which nixos-rebuild)" | grep -q "nixos-rebuild-ng"; then
    nixos-rebuild switch --flake .$REV --sudo
else
    nix shell nixpkgs#nixos-rebuild-ng -c nixos-rebuild-ng switch --flake . --sudo
fi
