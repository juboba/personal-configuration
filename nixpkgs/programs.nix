{ pkgs, builtins }:

let
  HOME_PATH = builtins.getEnv "HOME";
in with builtins; {
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

        window = {
          padding = { x = 10; y = 10; };
        };
      };
    };

    bash = {
      enable = true;
      initExtra = ''
        PATH=~/.emacs.d/bin:$PATH
        # Disable terminal suspension with Ctrl + s and Ctrl + q
        stty -ixon -ixoff

        # `cd` when quit ranger
        source ~/.config/ranger/shell_automatic_cd.sh

        # `cd` when quit nnn
        if [ -f ~/.config/nnn/plugins/quitcd.bash_zsh ]
        then
          source ~/.config/nnn/plugins/quitcd.bash_zsh
        fi

        export NNN_BMS='m:~/Projects/genially/mono;o:~/Documents/Org'

        # Greet with some fortune cookie | with lovely colors
        fortune | lolcat
      '' + (readFile ./dotfiles/functions.bash);
      historyIgnore = [ "ls" "cd" "exit" ];

      profileExtra = ''
        setxkbmap -option caps:escape
        if [ -e /home/juboba/.nix-profile/etc/profile.d/nix.sh ]; then . /home/juboba/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
        '';
      shellOptions =  [ "histappend" "checkwinsize" "extglob" "globstar" "checkjobs" "autocd" ];

      sessionVariables = {
        EDITOR = "vim";
      };

      shellAliases = import ./aliases;
    };

    bat.enable = true;

    emacs = {
      enable = true;
      extraPackages = epkgs: [ epkgs.emacsql-sqlite ] ;
      #package =
    };

    git = {
      enable = true;
      userEmail = "juboba@genial.ly";
      userName = "Julio Borja Barra";
      extraConfig = {
        github = {
          oauth-token = readFile ~/.oauth-token;
          user = "juboba";
        };

        init.defaultBranch = "main";

        merge.conflictstyle = "diff3";

        pull.ff = "only";

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
      theme = "${HOME_PATH}/.config/rofi/themes/User\ Themes/slate.rasi";
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

    vim = {
      enable = true;
      plugins = with pkgs.vimPlugins; [
        ale
        awesome-vim-colorschemes
        ctrlp
        emmet-vim
        fugitive
        nerdtree
        vim-airline
        vim-airline-themes
        vim-css-color
        vim-devicons
        vim-gitgutter
        vim-jsx-pretty
        vim-one
        vim-surround
      ];

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
        set showcmd

        let b:ale_fixers = ['prettier', 'eslint']
        let g:ale_completion_enabled = 1

        set omnifunc=ale#completion#OmniFunc
        let g:ale_completion_autoimport = 1

        let g:airline_theme='fruit_punch'
        let g:airline_powerline_fonts = 1

        xmap s <Plug>VSurround

        colorscheme OceanicNext
      '';
    };

    tmux = {
      baseIndex = 1;
      enable = true;
      extraConfig = builtins.readFile ./dotfiles/tmux.conf;
      historyLimit = 10000;
      keyMode = "vi";
      shortcut = "x";
      terminal = "screen-256color";
    };

    zathura = {
      enable = true;
    };
}
