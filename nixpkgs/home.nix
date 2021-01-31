{ pkgs, ... }:

let 
  HOME_PATH = builtins.getEnv "HOME";
  SCRIPT_PATH = "${HOME_PATH}/.scripts";
  BIN_PATH = "${HOME_PATH}/.bin";
  oh-my-tmux-rev = "53d7ce831127b6f1b6f1600b53213cb3060b7e6d";
  packages = import ./packages.nix;
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

  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./dotfiles/xmonad.hs;
    };
  };

  xsession.initExtra = ''
    # Set PATH
    PATH=${builtins.toString BIN_PATH}:${builtins.toString SCRIPT_PATH}:${builtins.toString HOME_PATH}/.emacs.d/bin:$PATH

    # Welcome sound:
    # mpv somesound.wav &

    # Set background:
    ${HOME_PATH}/.fehbg

    # Disable touchscreen
    totouch --off

    # Start applications
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

  home = {
    stateVersion = "20.09";

    username = "juboba";
    homeDirectory = /home/juboba;

    sessionVariables = {
      CM_LAUNCHER = "rofi";
    };

    keyboard.layout="us";
    keyboard.variant = "altgr-intl";

    packages = packages pkgs;

    # Extra configs
    file = {
      ".config/rofi/themes".source = (fetchFromGitHub {
        name = "rofi-themes";
        owner = "davatorium";
        repo = "rofi-themes";
        rev = "bfdde8e7912ad50a468c721b29b448c1ec5fa5e3";
        sha256 = "1k0nznqccc9y13x3pj6yd2j80nbnl3pyy8ihs91rf89gizb09w63";
      });

      "${SCRIPT_PATH}".source = (fetchFromGitHub {
        name = "scripts";
        owner = "juboba";
        repo = "scripts";
        rev = "33aa268d0b8dc3a8020478cc98e3955ae6cbac55";
        sha256 = "0hjq0dmx6ljjbp023r9x7sa0lyi0mjligqw2g9c47pjawqhvqp5l";
      });

    # Oh-my-tmux configuration takes over my tmux.conf file
      ".tmux.conf".text = builtins.readFile (fetchFromGitHub {
        name = "oh-my-tmux";
        owner = "gpakosz";
        repo = ".tmux";
        rev = oh-my-tmux-rev;
        sha256 = "12dsdxv7sy2fwlax5pwq2ahplmynlgb9y9j2cgwi0i45p0gphvhh";
        stripRoot = false;
      } + "/.tmux-${oh-my-tmux-rev}/.tmux.conf");

      # My oh-my-tmux config
      ".tmux.conf.local".source = ./dotfiles/oh-my-tmux.conf.local;
    };
  };

  xdg = {
    enable = true;

    configFile = {
      "doom" = {
        recursive = true;
        source = ./dotfiles/xdg-configs/doom;
      };

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
      initExtra = ''
        [[ $- != *i* ]] && return

        # Disable terminal suspension with Ctrl + s and Ctrl + q
        stty -ixon -ixoff

        # Greet with some fortune cookie | with lovely colors
        fortune | lolcat
      '' + (readFile ./dotfiles/functions.bash);
      historyIgnore = [ "ls" "cd" "exit" ];
      shellOptions =  [ "histappend" "checkwinsize" "extglob" "globstar" "checkjobs" "autocd" ];
      sessionVariables = {
        EDITOR = "vim";
      };
      shellAliases = import ./aliases;
    };

    bat.enable = true;

    git = {
      enable = true;
      userEmail = "juboba@genial.ly";
      userName = "Julio Borja Barra";
      extraConfig = {
        core = {
          excludesfile = "${HOME_PATH}/.gitignore";
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
        set editing-mode vi
        set show-mode-in-prompt on

        #set vi-ins-mode-string \1\e[48;5;33;1m\2 I \1\e[38;5;33;48;5m\2\1\e[0m\2
        set vi-ins-mode-string \1\e[6 q\2

        #set vi-cmd-mode-string \1\e[48;5;166;1m\2 N \1\e[38;5;166;48;5m\2\1\e[0m\2
        set vi-cmd-mode-string \1\e[2 q\2
      '';
    };

    rofi = {
      enable = true;
      theme = "${HOME_PATH}/.config/rofi/themes/User\ Themes/sidetab.rasi";
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

    zathura = {
      enable = true;
    };
  };

  services = {
    blueman-applet.enable = true;

    clipmenu.enable = true;

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
      inactiveDim = "0.5";
      #inactiveOpacity = "0.9";
      blur = true;
    };

    redshift = {
      enable = true;
      #provider = "geoclue2";

      brightness = {
        day = "1";
        night = "0.8";
      };

      latitude = "36";
      longitude = "-6";

      temperature = {
        day = 7700;
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
