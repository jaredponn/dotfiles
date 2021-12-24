#!/bin/bash
mkdir -p ~/.config/nvim
mkdir -p ~/.config/nixpkgs

rm -f ~/.config/nvim/init.vim
rm -f ~/.config/nixpkgs/config.nix
rm -f ~/.bashrc
rm -f ~/.xinitrc
rm -rf ~/.xmonad

ln -s ~/dotfiles/config/nvim/init.vim ~/.config/nvim/init.vim
ln -s ~/dotfiles/config/nixpkgs/config.nix ~/.config/nixpkgs/config.nix

ln -s ~/dotfiles/bashrc ~/.bashrc
ln -s ~/dotfiles/xinitrc ~/.xinitrc
ln -s ~/dotfiles/xmonad ~/.xmonad

