#!/usr/bin/env bash

if [ -n "$1" ]
then
    TARGET="--target-host $1 --ask-sudo-password"
fi

if [ -n "$2" ]
then
    REV="?rev=$(git rev-parse $2)"
fi

nixos-rebuild switch --flake .$REV --sudo $TARGET
