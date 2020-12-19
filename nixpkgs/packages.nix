{ pkgs, ... }:

let custom-st = import ./custom-st;

in with pkgs; [
  # Utils
  bc
  dragon-drop
  feh
  fusuma
  fzy
  imagemagick
  ispell
  ripgrep
  custom-st
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
  volumeicon
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
  yarn
]

