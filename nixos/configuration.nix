{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  boot = {
    # https://en.wikipedia.org/wiki/Magic_SysRq_key
    kernel.sysctl."kernel.sysrq" = 1;

    loader = {
      systemd-boot.enable = true;
      timeout = 1;
      efi.canTouchEfiVariables = true;
    };

    plymouth.enable = true;
  };

  time.timeZone = "Europe/Madrid";

  networking = {

    hostName = "faraday"; # Define your hostname.
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    interfaces.eno1.useDHCP = true;
    interfaces.wlp1s0.useDHCP = true;

    networkmanager.enable = true;

    useDHCP = false;
    # wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Configure network proxy if necessary
    # proxy.default = "http://user:password@proxy:port/";
    # proxy.noProxy = "127.0.0.1,localhost,internal.domain";
  };

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
    
    xserver = {
      enable = true;
      autorun = true;
    
      displayManager.lightdm.enable = true;
      #displayManager.lightdm.background = ./lightdm-background.jpg;

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "caps:escape";
    
      libinput = {
        enable = true;
        touchpad = {
          additionalOptions = ''MatchIsTouchpad "on"'';
          naturalScrolling = true;
          tapping = true;
        };
      };
    };

    # Enable the OpenSSH daemon.
    # openssh.enable = true;

    openvpn.servers = {
      genially = {
        autoStart = false;
        config = ''config /home/juboba/.config/vpn/juboba@genial.ly.developers.ovpn'';
        updateResolvConf = true;
      };
    };

    printing = {
      enable = true;
      drivers = [ pkgs.brlaser pkgs.brgenml1lpr pkgs.brgenml1cupswrapper ];
    };

    avahi.enable = true;
    #avahi.nssmdns = true;

  };

  sound = {
    enable = true;

    mediaKeys = {
      enable = true;
    };
  };

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };


  virtualisation = {
    docker.enable = true;

    virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };
  };
  
  users = {
    extraGroups.vboxusers.members = ["juboba"];

    users.juboba = {
      isNormalUser = true;
      extraGroups = [ "audio" "docker" "input" "wheel" ];
    };
  };

  environment.systemPackages = with pkgs; [
    gnupg
    ghc
    wget
    home-manager
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs = {
    gnupg.agent = {
      enable = true;
      #enableSSHSupport = true;
    };

    vim.defaultEditor = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

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

