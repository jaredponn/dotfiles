# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# You can see different options for this at `man configuration.nix`

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Custom nixos build for raspberry pis:
  # https://rbf.dev/blog/2020/05/custom-nixos-build-for-raspberry-pis/#flashing
  # For the different architectures:
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/system/boot/binfmt.nix
  # boot.binfmt.emulatedSystems = [ "armv7l-linux" ];

  networking.hostName = "pletbjerg"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.wireless.interfaces = [ "wlp3s0" ];  

  # DNS resolver
  networking.nameservers = [ 
        "8.8.8.8"  # google
        "8.8.4.4"  # google
        "1.1.1.1"  # cloudflare
    ];

  # Set your time zone.
  time.timeZone = "America/Edmonton";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp2s0.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.
  services.xserver = {
    # Sane defaults for X Server
    enable = true;
    autorun = false;
    displayManager.startx.enable = true;
    enableCtrlAltBackspace = true;

    # Extra keyboard layouts
    layout = "us,dvorak";
    # See 
    #   @
    #       vi $(nix-build --no-out-link '<nixpkgs>' -A xkeyboard_config)/etc/X11/xkb/rules/base.lst
    #   @
    xkbOptions = "grp:alt_space_toggle";

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;
    libinput.touchpad.tapping = false;

    # Xmonad for the window manager
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    # Wacom support
    wacom = {
      enable = true;
    };
  };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # 
  hardware.enableRedistributableFirmware = true;


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jared = {
    isNormalUser = true;
    initialPassword = "1234";
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    # Packages to use.. 
    packages = with pkgs; [
      # Web browsers:
      firefox
      chromium
      qutebrowser
      mpv

      # audio
      pulsemixer

      # Utilities:
      wget
      git
      zip
      pdfgrep
      unzip
      arandr
      execline
      xclip
      ping

      # Xournalpp
      xournalpp
      xournal

      # Latex / Zathura
      # texlive.combined.scheme-small
      # We need the full tex live normally
      texlive.combined.scheme-full
      zathura

      # screen shot
      flameshot

      # terminal
      alacritty

      # Windows manager / graphics
      xmobar
      paper-icon-theme

      # Viewing images
      imagemagick
      feh

      # additional man pages
      man-pages 
      man-pages-posix

      # Battery life
      acpi
    ];
  };


  # Basic extra programs
  programs = { 
    # Neovim 
    neovim = {
      defaultEditor = true;
      enable = true;
      viAlias = true;
      vimAlias = true;
    };

    # For screen brightness
    light.enable = true;
  };

  # Making the installation a little more minimal (removing useless nano) 
  # Pretty sure this is a bug..
  environment.defaultPackages = [];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    w3m
    firefox

    # Wacom
    xf86_input_wacom
  ];

  # Extra nix options

  # Some alias for the whole system
  environment.interactiveShellInit = ''
    alias nixosrc="sudo nvim /etc/nixos/configuration.nix"
    # alias dim="sudo light -S 30"
  '';

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # This is for SSD speed or something...
  services.fstrim.enable = true;


  # Enable the Cron
  services.cron = {
    enable = true;
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

