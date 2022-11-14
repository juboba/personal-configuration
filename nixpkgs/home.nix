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

      #passwordCommand = "get_pass gmail";
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
      config = ./dotfiles/xmonad.hs;
      enable = true;
      enableContribAndExtras = true;
    };

    initExtra = ''
      # Welcome sound:
      # mpv somesound.wav &

      # Set background:
      ${HOME_PATH}/.fehbg
  '';
  };


  home = {
    sessionVariables = {
      CM_LAUNCHER = "rofi";
    };

    keyboard = {
      layout="us";
      variant = "altgr-intl";
      options = [
        "caps:escape"
      ];
    };

    packages = (import ./packages.nix) pkgs;

    pointerCursor = {
      gtk.enable = true;
      name = "Nordzy-cursors";
      package = nordzy-cursor-theme;
      size = 64;
      x11.enable = true;
    };

    file = {
      ".bash_completion".text = builtins.readFile ./bash_completion/git.sh;

      ".local/reveal.js".source = (fetchFromGitHub {
        name = "revealjs";
        owner = "hakimel";
        repo = "reveal.js";
        rev = "4.1.0";
        sha256 = "10xhblbyw8mvak58d294hbxxnf5sq0akj6qldv7brgm6944zppm0";
      });

      ".local/yeelight-shell-scripts".source = (fetchFromGitHub {
        name = "yeelight-shell-scripts";
        owner = "hphde";
        repo = "yeelight-shell-scripts";
        rev = "50d34fb884235f5614a8ba8163bcdfb3cdc4e536";
        sha256 = "0yh6lgd341j7clz4h5xkmcj21iv68a0azamxc7arffydig6ah17f";
      });

      ".local/share/applications".source = ./dotfiles/applications;
      ".local/share/fonts".source = ./fonts;

      ".config/rofi/themes".source = (fetchFromGitHub {
        name = "rofi-themes";
        owner = "lr-tech";
        repo = "rofi-themes-collection";
        rev = "5ae9b23ef58893229b0df57ad750ad84801a632e";
        sha256 = "sha256-ecCQcDVWXpSilER99OROW9wutIq58llUGjFTn9rH2RM=";
      });

      ".xmobarrc".text = builtins.readFile ./dotfiles/xmobarrc;
    };
  };

  xdg = (import ./xdg.nix) { inherit pkgs; };
  programs = (import ./programs.nix) { inherit pkgs builtins; };
  services = (import ./services.nix) {};

  systemd.user = {
    services = {
      conky = {
        Unit = {
          Description = "A conky service";
        };

        Install = {
          WantedBy = ["graphical-session.target"];
        };

        Service = {
          Type="forking";
          ExecStart="${pkgs.conky}/bin/conky -d -c ${./dotfiles/xdg-configs/conky/config}";
        };
      };

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
