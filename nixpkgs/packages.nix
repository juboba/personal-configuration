{ pkgs, ... }:

let gitduck = (pkgs.callPackage ./gitduck {}).gitduck;
    nodemon = (pkgs.callPackage ./nodemon {}).nodemon;
    eslint_d = (pkgs.callPackage ./eslint_d {}).eslint_d;
    yarnWithNode10 = pkgs.yarn.overrideAttrs (oldAttrs: rec {
      buildInputs = with pkgs; [
        nodejs-10_x
      ];
});

in with pkgs; [
  # Utils
  bc
  brightnessctl
  conky
  dragon-drop
  dunst
  exa
  feh
  fd
  file
  fusuma
  fzf
  fzy
  gcalcli
  htop
  juboba-bin
  juboba-scripts
  imagemagick
  ispell
  nixfmt
  procs
  ripgrep
  simplescreenrecorder
  steam-run
  tmux
  tmuxp
  xournal
  zscroll

  # Inutils
  fortune
  lolcat

  # UI
  gsimplecal
  libnotify
  scrot
  slock
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
  mpv
  pamix
  python27Packages.pygments
  spotify
  spotifywm
  sxiv
  texlive.combined.scheme-full

  # Browser
  chromium
  firefox
  google-chrome
  qutebrowser

  # Communication
  discord
  slack
  tdesktop

  # Development
  (emacsWithPackages (epkgs: [ epkgs.emacsql-sqlite ]))
  #doom-emacs
  eslint_d
  gitduck
  gotty
  gsh
  highlight
  hugo
  jq
  ngrok
  nodePackages.node2nix
  nodejs-10_x
  nodemon
  peek
  pick-colour-picker
  python3
  robo3t
  shellcheck
  yarnWithNode10
]

