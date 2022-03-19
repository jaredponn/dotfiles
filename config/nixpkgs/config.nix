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
        cloc
        telnet
        valgrind
        file
        tree
        run
        gdb
        R
        bind # common network utilities

        # An example command:
        #   @
        #       ffmpeg -video_size 1024x768 -framerate 25 -f x11grab -i :0.0+100,200 -f alsa -ac 2 -i hw:0 output.mkv
        #   @
        # this will grab the image from the desktop starting with the pper-left
        # corner at x=100, y= 200, with a width and height of 1024x768...
        # You probably want:
        #   @
        #       pactl list short sources        # pick output
        #       ffmpeg -f x11grab -video_size 1920x1080 -i :0.0 -f pulse -ac 2 -i alsa_output.pci-0000_00_1f.3.analog-stereo.monitor -framerate 24 testout.mkv
        #   @
        ffmpeg

        # Xorg utilitiesA
        xorg.xmessage

        # Xournalpp
        xournalpp
        xournal

        # C/C++
        # gcc
        clang

        # Haskell
        stack
        (haskellPackages.ghcWithPackages (pkgs : [pkgs.vector pkgs.unordered-containers])
            )
        cabal-install
        haskellPackages.alex
        haskellPackages.happy

        # llvm (probably get a specific version)
        llvm_9

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
