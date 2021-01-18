{ pkgs, ... }:

let custom-st = import ./custom-st;

in with pkgs; [
  # Utils
  bc
  custom-st
  dragon-drop
  feh
  fd
  fusuma
  fzy
  htop
  imagemagick
  ispell
  nixfmt
  ripgrep
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
  highlight
  hugo
  jq
  nodejs-10_x
  peek
  pick-colour-picker
  robo3t
  shellcheck
  yarn
]

