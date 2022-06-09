{ pkgs, ... }:

let 
  nodejs-16_13_1 = (import (fetchTarball {
      name = "nixpkgs-node16_13_1";
      url = "https://github.com/NixOS/nixpkgs/archive/c82b46413401efa740a0b994f52e9903a4f6dcd5.tar.gz";
      sha256 = "13s8g6p0gzpa1q6mwc2fj2v451dsars67m4mwciimgfwhdlxx0bk";
    }) {}).nodejs-16_x;

    yarnWithNode16 = pkgs.yarn.overrideAttrs (oldAttrs: rec {
      buildInputs = with pkgs; [
        nodejs-16_13_1
      ];
    });

    nodemon = (pkgs.callPackage ./nodemon { inherit pkgs; }).nodemon;

    typescript-language-server = (pkgs.callPackage ./typescript-language-server { inherit pkgs; }).typescript-language-server;

    comma = with pkgs; callPackage (fetchFromGitHub {
      name = "comma";
      owner = "nix-community";
      repo = "comma";
      rev = "02e3e5545b0c62595a77f3d5de1223c536af0614";
      sha256 = "0qgg632ky6bnwkf9kh1z4f12dilkmdy0xgpal26g2vv416di04jq";
    }) { inherit pkgs; };

in with pkgs; [
  # Utils
  bc
  brightnessctl
  comma
  conky
  xdragon
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
  qsudo
  ripgrep
  simplescreenrecorder
  steam-run
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
  awscli2
  cypress
  docker-compose
  gcc
  gotty
  gsh
  highlight
  hugo
  jq
  kubectl
  ngrok
  nodejs-16_13_1
  yarnWithNode16
  nodePackages.node2nix
  nodemon
  peek
  pick-colour-picker
  python3
  robo3t
  shellcheck
  trayer
  typescript-language-server
  unzip
]

