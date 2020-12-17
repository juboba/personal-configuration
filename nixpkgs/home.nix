{ pkgs, ... }:

let 
  HOME_PATH = /home/juboba;
  custom-st = import ./custom-st;
  oh-my-tmux-rev = "53d7ce831127b6f1b6f1600b53213cb3060b7e6d";
in with pkgs; {

  # Email configuration
  accounts.email = {
    accounts.genially = {
      address = "juboba@genial.ly";
      aliases = ["juboba@genially.com"];
      flavor = "gmail.com";
      imap = {
        host = "imap.gmail.com";
        tls.enable = true;
      };
      mu.enable = true;
      offlineimap = {
        enable = true;
        extraConfig.account.autorefresh = 10;
      };
      passwordCommand = "get_pass gmail";
      primary = true;
      realName = "Julio Borja Barra";
    };
    certificatesFile = /etc/ssl/certs/ca-certificates.crt;
  };

  xresources.properties = {
    "*background" = "#303036";
    "*foreground" = "#fffaff";
  };

  xsession.enable = true;
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./dotfiles/xmonad.hs;
  };

  xsession.initExtra = ''
    # Set PATH
    PATH=${builtins.toString HOME_PATH}/.bin:${builtins.toString HOME_PATH}/.emacs.d/bin:$PATH

    # This is just a nasty hack to get sqlite working for emacs
    # it just works in a `nix-shell` so I ran one and kept the PATH saved in
    # this file here. Don't rely on it! since those files will be purged when I run
    # the garbage collector...
    PATH=$(cat ~/sqlite_shell_path):$PATH

    # Welcome sound:
    # mplayer somesound.wav &

    # Set background:
    ~/.fehbg

    # Disable touchscreen
    totouch --off

    # Start applications
    volumeicon &
    fusuma &               # Fusuma mouse gestures
  '';

  xsession.profileExtra = ''
    # Resolution
    xrandr --output eDP-1 --mode 1920x1080

    # Xresources
    xrdb -merge .Xresouces

    # Set blank screen timeout (in seconds)
    xset s 300
  '';

  xsession.pointerCursor = {
    defaultCursor = "left_ptr";
    name = "Numix-Cursor";
    package = numix-cursor-theme;
    size = 64;
  };

  home.stateVersion = "20.09";

  home.username = "juboba";
  home.homeDirectory = /home/juboba;

  home.sessionVariables = {
    CM_LAUNCHER = "rofi";
  };

  home.keyboard.layout="us";
  home.keyboard.variant = "altgr-intl";

  home.packages = [
      # Utils
      bc
      dragon-drop
      feh
      fusuma
      fzy
      imagemagick
      ispell
      ripgrep
      custom-st
      tmux
      tmuxp
      zscroll

      # Inutils
      fortune
      lolcat

      # UI
      gsimplecal
      libnotify
      scrot
      slock
      volumeicon
      xcalib
      xclip
      xdotool
      xmobar
      xorg.transset
      xsel

      # File System
      nnn
      ranger

      # Media
      mplayer
      spotify
      spotifywm
      sxiv
      texlive.combined.scheme-tetex
      python27Packages.pygments
      zathura

      # Browser
      chromium
      surf

      # Communication
      slack
      tdesktop

      # Development
      docker-compose
      highlight
      hugo
      jq
      nodejs-10_x
      peek
      pick-colour-picker
      robo3t
      yarn
  ];

  # Extra configs
  home.file = {
  # Oh-my-tmux configuration takes over my tmux.conf file
    ".tmux.conf".text = builtins.readFile (fetchFromGitHub rec {
      owner = "gpakosz";
      repo = ".tmux";
      rev = oh-my-tmux-rev;
      sha256 = "12dsdxv7sy2fwlax5pwq2ahplmynlgb9y9j2cgwi0i45p0gphvhh";
    } + "/.tmux-${oh-my-tmux-rev}/.tmux.conf");

    # My oh-my-tmux config
    ".tmux.conf.local".source = ./dotfiles/oh-my-tmux.conf.local;
  };

  xdg = {
    enable = true;

    configFile = {
      "fusuma" = {
        recursive = true;
        source = ./dotfiles/xdg-configs/fusuma;
      };

      "gsimplecal" = {
        recursive = true;
        source = ./dotfiles/xdg-configs/gsimplecal;
      };

      "ranger" = {
        recursive = true;
        source = ./dotfiles/xdg-configs/ranger;
      };

      "sxiv" = {
        recursive = true;
        source = ./dotfiles/xdg-configs/sxiv;
      };
    };
  };

  programs = with builtins; {
    bash = {
      enable = true;
      initExtra = readFile ./dotfiles/bash-it.bashrc;
    };

    bat.enable = true;

    git = {
      enable = true;
      userEmail = "juboba@genial.ly";
      userName = "Julio Borja Barra";
      extraConfig = {
        core = {
          excludesfile = "~/.gitignore";
        };
        github = {
          oauth-token = readFile ~/.oauth-token;
          user = "juboba";
        };
        pull = {
          ff = "only";
        };
      };
    };

    mu.enable = true;
    offlineimap.enable = true;

    readline = {
      enable = true;
      bindings = {
        "\\e[A" = "history-search-backward"; # arrow up
        "\\e[B" = "history-search-forward"; # arrow down
      };
      extraConfig = ''
        #set vi-ins-mode-string \1\e[48;5;33;1m\2 I \1\e[38;5;33;48;5m\2\1\e[0m\2
        #set vi-cmd-mode-string \1\e[48;5;166;1m\2 N \1\e[38;5;166;48;5m\2\1\e[0m\2

        set show-mode-in-prompt on
        set vi-ins-mode-string \1\e[6 q\2
        set vi-cmd-mode-string \1\e[2 q\2
      '';
    };

    rofi = {
      enable = true;
      theme = "~/Repositories/Configs/rofi-themes/slate.rasi";
    };

    starship = {
      enable = true;
      enableBashIntegration = true;
      settings = {
        add_newline = false;
        nix_shell = {
          impure_msg = "i";
          pure_msg = "p";
        };
      };
    };

    vim = {
      enable = true;
      plugins = with pkgs.vimPlugins; [ vim-airline vim-airline-themes vim-surround nerdtree ];
      settings = {
        expandtab = true;
        number = true;
        relativenumber = true;
        tabstop = 2;
      };
      extraConfig = ''
        set smarttab
        set softtabstop=0
        set shiftwidth=2
        set autoindent

        "let g:miramare_enable_italic = 1
        let g:miramare_disable_italic_comment = 1
        let g:airline_theme='bubblegum'

        colorscheme miramare
      '';
    };
  };

  services = {
    clipmenu.enable = true;

    dropbox = {
      enable = true;
      path = ~/Documents/Org/Dropbox;
    };

    dunst = {
      enable = true;
      settings = import ./dunst.nix;
    };

    grobi = {
      enable = true;
      rules = [
        {
          name = "Solo";
          outputs_disconnected = [ "DP-1" ];
          configure_single = "eDP-1@1920x1080";
          primary = true;
          atomic = true;
          execute_after = [
            "${pkgs.xorg.xrandr}/bin/xrandr --dpi 96"
            "${pkgs.xmonad-with-packages}/bin/xmonad --restart"
          ];
        }
        {
          name = "Home";
          outputs_connected = [ "DP-1" ];
          configure_single = "DP-1";
          primary = true;
          atomic = true;
          execute_after = [
            "${pkgs.xorg.xrandr}/bin/xrandr --dpi 96"
            "${pkgs.xmonad-with-packages}/bin/xmonad --restart"
          ];
        }
      ];
    };

    network-manager-applet.enable = true;

    picom = {
      enable = true;
      inactiveDim = "0.2";
      inactiveOpacity = "0.9";
      blur = true;
    };

    redshift = {
      enable = true;
      #provider = "geoclue2";

      brightness = {
        day = "1";
        night = "1";
      };

      latitude = "36";
      longitude = "-6";

      temperature = {
        day = 5500;
        night = 3700;
      };

      tray = true;
    };

    stalonetray = {
      enable = true;
      config = {
        transparent = false;
        geometry  = "1x1+1050+3";

        background = "#111b1e";
        icon_size = 16;
        kludges = "force_icons_size";
        grow_gravity = "W";
      };
    };
  };
}
