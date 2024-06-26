{ pkgs, ... }:

let
  HOME_PATH = builtins.getEnv "HOME";
in
with builtins; {
  programs = {
    alacritty = {
      enable = true;

      settings = {
        colors = {
          primary = {
            background = "#000000";
            foreground = "#dddddd";
          };
        };

        env = {
          WINIT_X11_SCALE_FACTOR = "1.0";
        };

        font = {
          size = 15;
        };

        "import" = [ "${HOME_PATH}/.config/alacritty/dracula/dracula.yml" ];

        window = {
          padding = { x = 10; y = 10; };
        };
      };
    };

    bash = {
      enable = true;

      initExtra = ''
        # Disable terminal suspension with Ctrl + s and Ctrl + q
        stty -ixon -ixoff

        # `cd` when quit ranger
        if [ -f ~/.config/ranger/shell_automatic_cd.sh ]
        then
          source ~/.config/ranger/shell_automatic_cd.sh
        fi

        # `cd` when quit nnn
        if [ -f ~/.config/nnn/plugins/quitcd.bash_zsh ]
        then
          source ~/.config/nnn/plugins/quitcd.bash_zsh
        fi

        export NNN_BMS='m:~/projects/genially/mono;o:~/Documents/Org'

        # Greet with some fortune cookie | with lovely colors
        fortune | lolcat
      '' + (readFile ./dotfiles/functions.bash);
      historyIgnore = [ "ls" "cd" "exit" ];

      profileExtra = ''
        setxkbmap -option caps:escape
      '';

      shellOptions = [ "histappend" "checkwinsize" "extglob" "globstar" "checkjobs" "autocd" ];

      shellAliases = import ./aliases;
    };

    bat.enable = true;

    emacs = {
      enable = true;
      package = pkgs.emacs29-gtk3;
      extraPackages = epkgs: [ epkgs.emacsql-sqlite ];
    };

    eza = {
      enable = true;

      enableBashIntegration = true;

      extraOptions = [
        "--group-directories-first"
      ];
    };

    git = {
      enable = true;

      userEmail = "juboba@genial.ly";
      userName = "Julio Borja Barra";
      extraConfig = {
        branch = {
          sort = "-committerdate";
        };

        core = {
          untrackedcache = true;
          fsmonitor = true;
        };

        github = {
          oauth-token = readFile /home/juboba/.oauth-token;
          user = "juboba";
        };

        gpg.format = "ssh";

        init.defaultBranch = "main";

        merge.conflictstyle = "diff3";

        ui.color = false;

        user.signinkey = "~/.ssh/id_rsa.pub";
      };

      ignores = [
        ".dir-locals.el"
        ".projectile"
        "TAGS"
        ".tern-project"
        "tsserver.log"
        "ti-.*.log"
        ".log"
      ];
    };

    home-manager = {
      enable = false;
    };

    mu.enable = true;
    offlineimap.enable = true;

    qutebrowser = {
      enable = true;
      loadAutoconfig = true;

      extraConfig = ''
        import  dracula.draw

        dracula.draw.blood(c, {
            'spacing': {
                'vertical': 6,
                'horizontal': 8
            }
        })
      '';
    };

    readline = {
      enable = true;

      bindings = {
        "\\e[A" = "history-search-backward"; # arrow up
        "\\e[B" = "history-search-forward"; # arrow down
      };

      variables = {
        editing-mode = "vi";
        show-mode-in-prompt = true;
        vi-cmd-mode-string = "\\1\\e[38;5;214m\\2 N \\1\\e[0m\\2";
        vi-ins-mode-string = "\\1\\e[38;5;27m\\2 I \\1\\e[0m\\2";
      };
    };

    rofi = {
      enable = true;
      theme = "sidebar";
    };

    starship = {
      enable = true;
      enableBashIntegration = true;

      settings = {
        add_newline = true;

        line_break = {
          disabled = true;
        };

        aws = {
          disabled = true;
        };

        nix_shell = {
          impure_msg = "i";
          pure_msg = "p";
        };
      };
    };

    neovim = {
      enable = false;

      defaultEditor = true;

      viAlias = true;
      vimAlias = true;
    };

    tmux = {
      enable = true;

      baseIndex = 1;
      extraConfig = builtins.readFile ./dotfiles/tmux.conf;
      historyLimit = 10000;
      keyMode = "vi";
      plugins = [
        {
          plugin = pkgs.tmuxPlugins.dracula;
          extraConfig = ''
            set -g @dracula-show-fahrenheit false
            set -g @dracula-show-battery false
            set -g @dracula-show-network false
          '';
        }
      ];
      shortcut = "x";
      terminal = "screen-256color";
    };

    xmobar = {
      enable = true;

      extraConfig = builtins.readFile ./dotfiles/xmobarrc;
    };

    zathura = {
      enable = true;
    };

    zellij = {
      enable = true;

      settings = {
        #default_mode = "locked";

        keybinds = {
          unbind = "Ctrl b";
        };

        theme = "juboba";

        themes = {
          juboba = {
            fg = [ 248 248 242 ];
            bg = [ 40 42 54 ];
            black = [ 0 0 0 ];
            red = [ 255 85 85 ];
            green = "#30bced";
            yellow = [ 241 250 140 ];
            blue = [ 98 114 164 ];
            magenta = [ 255 121 198 ];
            cyan = [ 139 233 253 ];
            white = [ 255 255 255 ];
            orange = [ 255 184 108 ];
          };
        };

        ui.pane_frames.rounded_corners = true;
      };
    };
  };
}
