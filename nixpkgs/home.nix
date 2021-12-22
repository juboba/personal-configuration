{ pkgs, ... }:

let 
  HOME_PATH = builtins.getEnv "HOME";
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

      passwordCommand = "cat /home/juboba/.gmail_pass";
      primary = true;
      realName = "Julio Borja Barra";
    };

    certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
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

    initExtra = ''
      # Welcome sound:
      # mpv somesound.wav &

      # Set background:
      ${HOME_PATH}/.fehbg

      # Disable touchscreen
      totouch --off

      # Start applications
      fusuma &
  '';

    pointerCursor = {
      defaultCursor = "left_ptr";
      name = "Numix-Cursor";
      package = numix-cursor-theme;
      size = 64;
    };
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

    packages = (import ./packages.nix) pkgs;

    # Extra configs
    file = {
      /*
      ".emacs.d/init.el".text = ''
            (load "default.el")
      '';
      */
      ".bash_completion".text = builtins.readFile ./bash_completion/git.sh;

      ".local/reveal.js".source = (fetchFromGitHub {
        name = "revealjs";
        owner = "hakimel";
        repo = "reveal.js";
        rev = "4.1.0";
        sha256 = "10xhblbyw8mvak58d294hbxxnf5sq0akj6qldv7brgm6944zppm0";
      });

      ".local/share/applications".source = ./dotfiles/applications;
      ".local/share/fonts".source = ./fonts;

      ".config/rofi/themes".source = (fetchFromGitHub {
        name = "rofi-themes";
        owner = "davatorium";
        repo = "rofi-themes";
        rev = "bfdde8e7912ad50a468c721b29b448c1ec5fa5e3";
        sha256 = "1k0nznqccc9y13x3pj6yd2j80nbnl3pyy8ihs91rf89gizb09w63";
      });

      ".xmobarrc".text = builtins.readFile ./dotfiles/xmobarrc;
    };
  };

  xdg = (import ./xdg.nix) { pkgs = pkgs; };
  programs = (import ./programs.nix) { pkgs = pkgs; builtins = builtins; };
  services = (import ./services.nix) {};

  systemd.user = {
    services = {
      syncmail = {
        Unit = {
          Description = "Sync email and index with mu";
        };

        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.offlineimap}/bin/offlineimap -o && ${pkgs.mu}/bin/mu index";
          #ExecStartPost = "${pkgs.mu}/bin/mu index";
          SuccessExitStatus = "0 1";
        };
      };
    };

    timers = {
      syncmail = {
        Unit = {
          Description = "Schedule syncing email and indexing with mu";
        };
        Timer = {
          Unit = "syncmail.service";
          OnCalendar = "hourly";
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
      };
    };
  };
}
