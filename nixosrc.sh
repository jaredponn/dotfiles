#!/bin/sh

# Simple script for copying the configuration.nix over to the location it should be.
sudo sh -c 'rm /etc/nixos/configuration.nix; cp ./configuration.nix /etc/nixos/configuration.nix'

# switch 
sudo sh -c 'nixos-rebuild switch'
