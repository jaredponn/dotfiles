# To install this in the environment, run @nix-env -iA nixpkgs.jaredPkgs@
# Actually only this works: @nix-env -f '<nixpkgs>' -iA jaredPkgs@
# To explore what has been installed, look through @~/.nix-profile/@
# https://nixos.org/manual/nixpkgs/stable/
{
  packageOverrides = pkgs : with pkgs; {
    jaredPkgs = buildEnv {
      name = "jaredPkgs";
      paths = [
	    # Neovim
        (neovim.override {
	      viAlias = true;
	      vimAlias = true;
          configure = {
  	        customRC = builtins.readFile ~/dotfiles/config/nvim/init.vim;
  	      };
        })

        # Common utilities
        gnumake
        strace

        # Xournalpp
        xournalpp
        xournal

        # Haskell
        stack
        ghc
        cabal-install
        haskellPackages.alex
        haskellPackages.happy

        # Latex
        texlive.combined.scheme-full
        zathura

        # Screen shot
        flameshot

        # Viewing images
        imagemagick
        feh

        # man pages
        man-pages 
        man-pages-posix
      ];

      # Man pages
      pathsToLink = ["/share" "/bin"];
      extraOutputsToInstall = ["man" "doc"];
    };
  };
}
