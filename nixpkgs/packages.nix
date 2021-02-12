{ pkgs, ... }:

let custom-st = import ./custom-st;
    gitduck = (pkgs.callPackage ./gitduck {}).gitduck;
    nodemon = (pkgs.callPackage ./nodemon {}).nodemon;

in with pkgs; [
  # Utils
  bc
  custom-st
  dragon-drop
  exa
  feh
  fd
  file
  fusuma
  fzy
  htop
  imagemagick
  ispell
  nixfmt
  ripgrep
  simplescreenrecorder
  steam-run
  tmux
  tmuxp
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
  surf

  # Communication
  discord
  slack
  tdesktop

  # Development
  gitduck
  highlight
  hugo
  jq
  nodejs-14_x
  nodemon
  nodePackages.node2nix
  peek
  pick-colour-picker
  python3
  robo3t
  shellcheck
  yarn
]

