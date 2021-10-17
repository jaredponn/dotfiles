#!/bin/sh

# Run flameshot for screen shots in the background
flameshot &

# XMonad windows manager
exec xmonad
