{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "jared";
  home.homeDirectory = "/home/jared";

  # User packages
  home.packages = with pkgs; [
    # Web browsers:
    firefox
    chromium
    qutebrowser

    # Utilities:
    wget
    git
    zip
    arandr
    execline

    # Haskell
    stack
    ghc

    # Xournalpp
    xournalpp
    xournal

    # Latex / Zathura
    texlive.combined.scheme-small
    zathura

    # screen shot
    flameshot

    # terminal
    alacritty

    # Windows manager / graphics
    xmobar
    paper-icon-theme
    ];

  # XMonad
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ~/dotfiles/xmonad/xmonad.hs;
    extraPackages = self: [ ];
  };

  # xinitrc
  home.file.".xinitrc".text = builtins.readFile ~/dotfiles/xinitrc;

  # Neovim
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    extraConfig = builtins.readFile ~/dotfiles/config/nvim/init.vim;
  };

  # Bashrc
  programs.bash = {
    enable = true;
    initExtra = builtins.readFile ~/dotfiles/bashrc;
  };


  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
