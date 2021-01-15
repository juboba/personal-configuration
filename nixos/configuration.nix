# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  environment.variables = { EDITOR = "vim"; };

  nixpkgs.config.allowUnfree = true;

  # Boot config
  boot = {
    loader = {
      systemd-boot.enable = true;
      timeout = 1;
      efi.canTouchEfiVariables = true;
    };

    plymouth.enable = true;
  };

  # Set your time zone.
  time.timeZone = "Europe/Madrid";

  networking = {
    hostName = "faraday"; # Define your hostname.
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces.wlp2s0.useDHCP = true;
    networkmanager.enable = true;
    # wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  services = {
    blueman.enable = true;
    cron.enable = true;
    geoclue2.enable = true;
    
    # Configure keymap in X11
    xserver = {
      enable = true;
      autorun = true;
    
      displayManager.lightdm.enable = true;
      displayManager.lightdm.background = /home/juboba/Pictures/Wallpapers/galaxy-wallpapers-20.jpg;

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    
      # Keyboard
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "caps:escape";
    
      # Enable touchpad support (enabled default in most desktopManager).
      libinput = {
        enable = true;
        naturalScrolling = true;
        tapping = true;
        additionalOptions = ''MatchIsTouchpad "on"'';
      };
    };

    # Enable the OpenSSH daemon.
    # openssh.enable = true;

    # Enable CUPS to print documents.
    printing = {
      enable = true;
      drivers = [ pkgs.brlaser pkgs.brgenml1lpr pkgs.brgenml1cupswrapper ];
    };

    avahi.enable = true;
    #avahi.nssmdns = true;

  };

  # Enable sound.
  sound.enable = true;

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  # Enable docker
  virtualisation.docker.enable = true;
  
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.juboba = {
    isNormalUser = true;
    extraGroups = [ "audio" "docker" "input" "wheel" ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    gnupg
    ghc
    wget
    vim (emacsWithPackages (epkgs: [ epkgs.emacsql-sqlite ]))
    firefox
    home-manager
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

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
  system.stateVersion = "20.09"; # Did you read the comment?

}

