#!/usr/bin/env bash

nixos-rebuild switch --flake .#$(hostname) --sudo
