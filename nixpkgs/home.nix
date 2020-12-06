{ pkgs, ... }:

with pkgs; {
  xsession.enable = true;
  xsession.windowManager.command = "xmonad";

  xsession.initExtra = ''
    # Set PATH
    PATH=/home/juboba/.bin:$PATH

    # Welcome sound:
    #mpv /usr/share/sounds/ubuntu/stereo/desktop-login.ogg &

    # Set background:
    ~/.fehbg

    # Start key bindings
    xbindkeys

    # Disable touchscreen
    totouch --off

    # Start applications
    picom -i 0.90 &  # Composite Manager
    dunst &               # Notification daemon
    stalonetray &         # Stand Alone Tray
    #pasystray &           # Pulseaudio system tray icon
    volumeicon &
    #nm-applet &           # Network-manager applet
    fusuma &              # Fusuma mouse gestures
    #xfce4-clipman &       # Clipboard manager
    #terminal -e jmux &          # Launch a terminal with tmux, because why not?
  '';

  xsession.profileExtra = ''
    # Resolution
    xrandr --output eDP-1 --mode 1920x1080

    # Xresources
    xrdb -merge .Xresouces

    # Set cursor:
    xsetroot -cursor_name left_ptr

    # Set blank screen timeout (in seconds)
    xset s 300
  '';

  home.packages = [
      # Utils
      bc
      dragon-drop
      dunst
      feh
      fusuma
      fzy
      imagemagick
      mu
      ripgrep
      st
      tmux
      tmuxp
      zscroll

      # Inutils
      fortune
      lolcat

      # UI
      gsimplecal
      libnotify
      picom
      rofi
      scrot
      slock
      stalonetray
      volumeicon
      xbindkeys
      xcalib
      xclip
      xmobar
      xorg.transset
      xsel

      # File System
      dropbox
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
      sqlite
      yarn
  ];

  programs = with builtins; {
    bash = {
      enable = true;
      initExtra = readFile /home/juboba/Repositories/Configs/bash-it.bashrc;
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
          oauth-token = readFile /home/juboba/.oauth-token;
          user = "juboba";
        };
      };
    };

    vim = {
      enable = true;
      plugins = with pkgs.vimPlugins; [ vim-airline vim-surround nerdtree ];
      settings = {
        expandtab=true;
        number=true;
        relativenumber=true;
        tabstop=2;
      };
      extraConfig = ''
        set smarttab
        set softtabstop=0
        set shiftwidth=2
        set autoindent

        "let g:miramare_enable_italic = 1
        let g:miramare_disable_italic_comment = 1

        colorscheme miramare
      '';
    };
  };
}
