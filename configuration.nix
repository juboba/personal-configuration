{ nixpkgs, nixpkgs-unstable }: { config, pkgs, ... }:

{
  amd-controller = {
    enable = true;

    udev.enable = true;

    processor = "4800H";

    runAsAdmin = {
      enable = true;

      user = "juboba";
    };

    powerManagement = {
      enable = true;

      awakeMode = "slow";
      cpuFreqGovernor = "powersave";
      powerUpCommandsDelay = 60;
    };

    thermald.enable = true;
  };

  boot = {
    # https://en.wikipedia.org/wiki/Magic_SysRq_key
    kernel.sysctl."kernel.sysrq" = 1;

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
      timeout = 1;
    };

    plymouth.enable = false;
  };

  environment.sessionVariables.FLAKE = "/home/juboba/repositories/personal-configuration";

  environment.systemPackages = with pkgs; [
    gnupg
    (ghc.withPackages (hpkgs: with hpkgs; [ xmonad xmonad-contrib X11 ]))
    nh
    wget
    home-manager
  ];

  hardware.pulseaudio = {
    enable = false;
    package = pkgs.pulseaudioFull;
  };

  imports = [
    #./nvim
  ];

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

  nixpkgs.overlays = [
    (final: prev: { qutebrowser = prev.qutebrowser.override { enableWideVine = true; }; })
  ];

  nixpkgs.config = {
    allowUnfree = true;
    permittedInsecurePackages = [
      "nix-2.15.3"
    ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs = {
    gnupg.agent.enable = true;
    hyprland.enable = true;
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
    displayManager.sddm.enable = false;
    geoclue2.enable = true;
    gpm.enable = true;

    openvpn.servers = {
      genially = {
        autoStart = false;
        config = ''config /home/juboba/.config/vpn/juboba@genial.ly.developers.ovpn'';
        updateResolvConf = true;
      };
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      jack.enable = true;
      pulse.enable = true;
    };

    printing = {
      enable = true;
      drivers = [ pkgs.brlaser pkgs.brgenml1lpr pkgs.brgenml1cupswrapper ];
    };

    tlp.enable = false;
    power-profiles-daemon.enable = true;

    auto-cpufreq = {
      enable = false;

      settings = {
        battery = {
          governor = "powersave";
          turbo = "never";
        };
        charger = {
          governor = "performance";
          turbo = "auto";
        };
      };
    };

    xserver = {
      enable = true;
      autorun = true;

      displayManager.lightdm = {
        enable = true;
        background = ./lightdm-background.jpg;
      };

      desktopManager.plasma5.enable = false;

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

      xkb = {
        layout = "us";

        variant = "altgr-intl";
        options = "caps:escape";
      };
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

  systemd = {
    extraConfig = ''
      DefaultLimitNOFILE=65535
    '';
    user.extraConfig = ''
      DefaultLimitNOFILE=65535
    '';
  };

  time.timeZone = "Europe/Madrid";

  users = {
    extraGroups.vboxusers.members = [ "juboba" ];

    users.juboba = {
      extraGroups = [ "pulse-access" "docker" "input" "wheel" ];
      isNormalUser = true;
    };

    users.guvova = {
      extraGroups = [ "pulse-access" "docker" "input" "wheel" ];
      isNormalUser = true;
    };
  };

  virtualisation = {
    docker.enable = true;

    virtualbox.host = {
      enable = false;
      enableExtensionPack = true;
    };
  };
}
