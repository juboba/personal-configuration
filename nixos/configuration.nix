{ nixpkgs }: { config, pkgs, ... }:

{
  amd-controller = {
    enable = true;

    udev.enable = true;

    # processor = "4800H";

    # power-saving = {
    #   ac = true;
    #   batt = true;
    # };

    # temperature = 95;
  };

  boot = {
    # https://en.wikipedia.org/wiki/Magic_SysRq_key
    kernel.sysctl."kernel.sysrq" = 1;

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
      timeout = 1;
    };

    plymouth.enable = true;
  };

  environment.systemPackages = with pkgs; [
    gnupg
    ghc
    wget
    home-manager
  ];

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  home-manager.useGlobalPkgs = true;

  networking = {
    hostName = "faraday"; # Define your hostname.
    interfaces.eno1.useDHCP = true;
    interfaces.wlp1s0.useDHCP = true;
    networkmanager.enable = true;
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
  };

  nix = {
    extraOptions = "experimental-features = nix-command flakes";
    package = pkgs.nixFlakes;
    registry.nixpkgs.flake = nixpkgs;
    #nixPath = ["nixos-config=/home/juboba/repositories/personal-configuration/nixos/configuration.nix"];
  };

  nixpkgs.config.allowUnfree = true;

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs = {
    gnupg.agent.enable = true;
    vim.defaultEditor = true;
  };

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  services = {
    avahi.enable = true;
    #avahi.nssmdns = true;
    blueman.enable = true;
    cron.enable = true;
    geoclue2.enable = true;
    gpm.enable = true;
    
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

    xserver = {
      enable = true;
      autorun = true;

      displayManager.lightdm = {
        enable = true;
        background = ./lightdm-background.jpg;
      };

      layout = "us";

      libinput = {
        enable = true;

        touchpad = {
          additionalOptions = ''MatchIsTouchpad "on"'';
          naturalScrolling = true;
          tapping = true;
        };
      };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      xkbVariant = "altgr-intl";
      xkbOptions = "caps:escape";
    };

  };

  sound = {
    enable = true;

    mediaKeys = {
      enable = true;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

  time.timeZone = "Europe/Madrid";

  users = {
    extraGroups.vboxusers.members = ["juboba"];

    users.juboba = {
      extraGroups = [ "pulse-access" "docker" "input" "wheel" ];
      isNormalUser = true;
    };
  };

  virtualisation = {
    docker.enable = true;

    virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };
  };
}
