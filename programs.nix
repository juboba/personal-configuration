{ pkgs, ... }:

let
  HOME_PATH = builtins.getEnv "HOME";
in with builtins; {
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
        PATH=~/.bin:~/.config/doom/bin:$PATH
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

      shellOptions =  [ "histappend" "checkwinsize" "extglob" "globstar" "checkjobs" "autocd" ];

      shellAliases = import ./aliases;
    };

    bat.enable = true;

    emacs = {
      enable = true;
      extraPackages = epkgs: [ epkgs.emacsql-sqlite ] ;
    };

    git = {
      enable = true;

      userEmail = "juboba@genial.ly";
      userName = "Julio Borja Barra";
      extraConfig = {
        github = {
          oauth-token = readFile /home/juboba/.oauth-token;
          user = "juboba";
        };

        init.defaultBranch = "main";

        merge.conflictstyle = "diff3";

        ui.color = false;
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
        add_newline = false;

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
      enable = true;

      defaultEditor = true;

      plugins = with pkgs.vimPlugins; [
        ale
        awesome-vim-colorschemes
        ctrlp
        emmet-vim
        fugitive
        nerdtree
        nnn-vim
        vim-airline
        vim-airline-themes
        vim-css-color
        vim-devicons
        vim-gitgutter
        vim-jsx-pretty
        vim-one
        vim-surround
        vim-nix
      ];

      extraConfig = ''
        set autoindent
        set expandtab
        set number
        set omnifunc=ale#completion#OmniFunc
        set relativenumber
        set shiftwidth=2
        set showcmd
        set smartcase
        set smarttab
        set softtabstop=0
        set tabstop=2
        set clipboard=unnamedplus

        let b:ale_fixers = ['prettier', 'eslint']
        let g:ale_completion_enabled = 1

        let g:ale_completion_autoimport = 1

        let g:airline_theme='fruit_punch'
        let g:airline_powerline_fonts = 1

        xmap s <Plug>VSurround

        colorscheme OceanicNext
      '';

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
        default_mode = "locked";

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
      };
    };
  };
}
