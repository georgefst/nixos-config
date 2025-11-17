#!/usr/bin/env bash

if [ -n "$1" ]
then
    REV="?rev=$(git rev-parse $1)"
fi

nixos-rebuild switch --flake .$REV --sudo
