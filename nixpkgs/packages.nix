{ pkgs, ... }:

let gitduck = (pkgs.callPackage ./gitduck {}).gitduck;
    nodemon = (pkgs.callPackage ./nodemon {}).nodemon;
    eslint_d = (pkgs.callPackage ./eslint_d {}).eslint_d;

in with pkgs; [
  # Utils
  bc
  dragon-drop
  exa
  feh
  fd
  file
  fusuma
  fzf
  fzy
  htop
  imagemagick
  ispell
  nixfmt
  procs
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
  qutebrowser

  # Communication
  discord
  slack
  tdesktop

  # Development
  eslint_d
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

