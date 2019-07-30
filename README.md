# Jared's Config Files

This repository contains the config files for my Linux set up. I run Void Linux as my daily driver.

## Features
* XMonad as the windows manager
* Sakura for the terminal emulator
* NeoVim as the text editor
* Fish instead of bash
* Mpd and ncmpcpp for music

## Fast Set-Up
Assuming all dependencies and programs have been installed, run the following commands to link the config files. 

```bash
# change to the home directory
cd ~
# clone repository in the home directory
git clone https://github.com/jaredponn/Jared-s-Config-Files.gi://github.com/jaredponn/Jared-s-Config-Files.git 
# changing directory to the repository
cd Jared-s-Config-Files
# update the sub modules
git submodule update --init --recursive
# running the file to symlink the config files to this repository
./link.sh
```

## Notes
 * I followed [this guy's](https://brianbuccola.com/how-to-install-xmonad-and-xmobar-via-stack/) tutorial to install XMonad with stack. Here's a short summary of what to do to get this repo working:
 ** Install stack.
 ** Install GHC with stack by running `stack setup`.
 ** Go to the `.xmonad` directory.
 ** Run `stack init` so it'll generate the `stack.yaml` file.
 ** Run `xmonad --recompile` and `xmonad --restart` to recompile and restart xmonad.

 * To get Haskell stack working with Void Linux, I got an error message of 
 ```bash
 No setup information found for ghc-8.4.3 on your platform.
 This probably means a GHC bindist has not yet been added for OS key 'linux64-ncurses6', 'linux64-tinfo6'.
 ```
 To fix this, go to `/lib` and run `# sudo ln -s libncursesw.so.6 libtinfo.so.6`.

 * Chromium's borders do not play nicely with XMonad's borders provided. To get around this, go to `Settings` and find `Use system title bar and borders`. Enable that setting and it should work.
 
 

