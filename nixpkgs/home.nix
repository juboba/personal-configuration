{ pkgs, ... }:

let 
  HOME_PATH = builtins.getEnv "HOME";
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

  xsession.enable = true;
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./dotfiles/xmonad.hs;
  };

  xsession.initExtra = ''
    # Set PATH
    PATH=${builtins.toString HOME_PATH}/.bin:${builtins.toString HOME_PATH}/.scripts:${builtins.toString HOME_PATH}/.emacs.d/bin:$PATH

    # This is just a nasty hack to get sqlite working for emacs
    # it just works in a `nix-shell` so I ran one and kept the PATH saved in
    # this file here. Don't rely on it! since those files will be purged when I run
    # the garbage collector...
    PATH=$(cat ${HOME_PATH}/sqlite_shell_path):$PATH

    # Welcome sound:
    # mpv somesound.wav &

    # Set background:
    ${HOME_PATH}/.fehbg

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

  home.packages = packages pkgs;

  # Extra configs
  home.file = {
    ".scripts".source = (fetchFromGitHub {
      name = "scripts";
      owner = "juboba";
      repo = "scripts";
      rev = "16fff37cf1f30d372a23f0a41d56de7dd4683a3d";
      sha256 = "0z9pakdwyxgcm8fypwzg22nf4fkndzfazhfd9haxg82razng2070";
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

      "volumeicon" = {
        recursive = true;
        source = ./dotfiles/xdg-configs/volumeicon;
      };
    };
  };

  programs = with builtins; {
    bash = {
      enable = true;
      initExtra = 
      (readFile ./dotfiles/functions.bash) +
  ''
        fortune | lolcat
      '';
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
      theme = "${HOME_PATH}/Repositories/Configs/rofi-themes/slate.rasi";
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
        night = "1";
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
