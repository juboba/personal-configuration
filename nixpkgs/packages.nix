{ pkgs, ... }:

let gitduck = (pkgs.callPackage ./gitduck {}).gitduck;
    nodemon = (pkgs.callPackage ./nodemon {}).nodemon;
    typescript-language-server = (pkgs.callPackage ./typescript-language-server {}).typescript-language-server;
    yarnWithNode10 = pkgs.yarn.overrideAttrs (oldAttrs: rec {
      buildInputs = with pkgs; [
        nodejs-10_x
      ];
    });

    comma = with pkgs; callPackage (fetchFromGitHub {
      name = "comma";
      owner = "nix-community";
      repo = "comma";
      rev = "02e3e5545b0c62595a77f3d5de1223c536af0614";
      sha256 = "0qgg632ky6bnwkf9kh1z4f12dilkmdy0xgpal26g2vv416di04jq";
    }) {};

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
  (emacsWithPackages (epkgs: [ epkgs.emacsql-sqlite ]))
  #doom-emacs
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
  typescript-language-server
  yarnWithNode10
]

