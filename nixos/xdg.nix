{ pkgs, ... }:

with pkgs; {
  xdg = {
    enable = true;

    configFile = {
      "alacritty/dracula" = {
        recursive = true;

        source = fetchFromGitHub {
          name = "dracula-theme";
          owner = "dracula";
          repo = "alacritty";
          rev = "77aff04b9f2651eac10e5cfa80a3d85ce43e7985";
          sha256 = "sha256-eJkVxcaDiIbTrI1Js5j+Nl88gawTE/mfVjstjqQOOdU=";
        };
      };

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

      "nnn" = {
        recursive = true;
        source = ./dotfiles/xdg-configs/nnn;
      };

      "qutebrowser/dracula" = {
        recursive = true;

        source = fetchFromGitHub {
          name = "dracula-theme";
          owner = "dracula";
          repo = "qutebrowser-dracula-theme";
          rev = "791de19ce6a43f0fa52405eea57cba43b4c05a22";
          sha256 = "sha256-BXTvYFZnzEDlNEOTaWm4m8MEelVrRsUkNdwYKxaxw/g=";
        };
      };

      "qutebrowser/config.py".source = ./dotfiles/xdg-configs/qutebrowser/config.py;

      "ranger" = {
        recursive = true;
        source = ./dotfiles/xdg-configs/ranger;
      };

      "rofi/plugins/rofiemoji".source = fetchFromGitHub {
        name = "rofiemoji";
        owner = "nkoehring";
        repo = "rofiemoji";
        rev = "ad61572830c9d3c00e30eec078d46dad3cfdb4a2";
        sha256 = "16rhb2cs8cqwflkcyw5dr77alp5wik4bv1dg66m4hkgcplxv0dx0";
      };

      "sxiv" = {
        recursive = true;
        source = ./dotfiles/xdg-configs/sxiv;
      };

      "tmux/plugins/prefix-highlight".source = fetchFromGitHub {
        name = "tmux-prefix-highlight";
        owner = "tmux-plugins";
        repo = "tmux-prefix-highlight";
        rev = "15acc6172300bc2eb13c81718dc53da6ae69de4f";
        sha256 = "08rkflfnynxgv2s26b33l199h6xcqdfmlqbyqa1wkw7h85br3dgl";
      };

      "tmux/themes".source = fetchFromGitHub {
        name = "tmux-themepack";
        owner = "jimeh";
        repo = "tmux-themepack";
        rev = "7c59902f64dcd7ea356e891274b21144d1ea5948";
        sha256 = "1kl93d0b28f4gn1knvbb248xw4vzb0f14hma9kba3blwn830d4bk";
      };
    };

    desktopEntries = {
      browser = {
        categories =  [ "Network" "WebBrowser" ];
        comment = "Pick to browse URL";
        exec = "pick-browser %U";
        genericName = "Browser";
        icon = "browser";
        name = "browser";
        terminal = false;
        type = "Application";
      };

      devium = {
        #startupWMClass = "Devium";
        #version = 1.0;
        comment = "Self destructing Chromium browser with predefined profile";
        exec = "devium %U";
        genericName = "Development Chromium";
        icon = "chromium";
        name = "Devium";
        terminal = false;
        type = "Application";
      };

      emacs = {
        exec = "emacs";
        name = "Emacs";
        noDisplay = true;
      };

      emacsclient = {
        comment = "Edit text";
        exec = "emacsclient -c %F";
        genericName = "Text editor";
        icon = "emacs";
        name = "Emacs";
        type = "Application";
      };

      gsh = {
        comment = "Genially dev-env";
        exec = "terminal gsh";
        genericName = "Genially Dev Env";
        icon = "screen";
        name = "gsh";
        terminal = false;
        type = "Application";
      };

      jmux = {
        comment= " My entry point to Tmux";
        exec = "jmux";
        genericName = "JMux";
        icon = "tmux";
        name = "JMux";
        terminal = false;
        type = "Application";
      };

      smod = {
        comment = "Select screen configuration";
        exec = "smod";
        icon = "screen";
        name = "Screen Mode";
        terminal = false;
        type = "Application";
      };

      sndsel = {
        comment = "Select sound output";
        exec = "sndsel";
        icon = "screen";
        name = "Sound output select";
        terminal = false;
        type = "Application";
      };

      spotify = {
        #mimeType = "x-scheme-handler/spotify;";
        #startupWMClass = "spotify";
        #tryExec = "spotifywm";
        categories = [ "Audio" "Music" "Player" "AudioVideo" ];
        exec = "spotifywm %U";
        genericName = "Music Player";
        icon = "spotify-client";
        name = "Spotify";
        terminal = false;
        type = "Application";
      };

    };

    mimeApps = {
      enable = true;

      defaultApplications =
        let
          browser = "browser.desktop";
          google-chrome = "google-chrome.desktop";
          qutebrowser = "org.qutebrowser.qutebrowser.desktop";
          slack = "org.pwmt.zathura.desktop";
          sxiv = "sxiv.desktop";
          zathura = "org.pwmt.zathura.desktop";
        in {
          "application/pdf" = zathura;
          "application/x-extension-htm" = browser;
          "application/x-extension-html" = browser;
          "application/x-extension-shtml" = browser;
          "application/x-extension-xht" = browser;
          "application/x-extension-xhtml" = browser;
          "application/xhtml+xml" = browser;

          "image/png" = sxiv;
          "text/html" = browser;

          "x-scheme-handler/chrome" = browser;
          "x-scheme-handler/http" = browser;
          "x-scheme-handler/https" = browser;
          "x-scheme-handler/slack" = slack;

        };
    };

    userDirs = {
      enable = true;

      desktop = "$HOME/desktop";
      documents = "$HOME/documents";
      download = "$HOME/downloads";
      music = "$HOME/music";
      pictures = "$HOME/pictures";
      publicShare = "$HOME/public";
      templates = "$HOME/templates";
      videos = "$HOME/videos";

    };
  };
}
