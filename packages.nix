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

    comma = with pkgs; callPackage (fetchFromGitHub {
      name = "comma";
      owner = "nix-community";
      repo = "comma";
      rev = "02e3e5545b0c62595a77f3d5de1223c536af0614";
      sha256 = "0qgg632ky6bnwkf9kh1z4f12dilkmdy0xgpal26g2vv416di04jq";
    }) { inherit pkgs; };

in with pkgs; {
  home.packages = [
    # Utils
    bc
    brightnessctl
    comma
    conky
    xdragon
    dunst
    feh
    fd
    file
    fzf
    fzy
    gcalcli
    htop
    imagemagick
    ispell
    juboba-bin
    nixfmt
    procs
    qsudo
    ripgrep
    simplescreenrecorder
    translate-shell
    unzip
    xournal
    zscroll

    # Inutils
    fortune
    lolcat

    # UI
    eww
    gh
    gsimplecal
    libnotify
    scrot
    slock
    wmctrl
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
    pulseaudio
    python39Packages.pygments
    spotifywm
    sxiv
    texlive.combined.scheme-full
    playerctl

    # Browser
    python310Packages.adblock
    firefox
    google-chrome

    # Communication
    discord
    slack
    kotatogram-desktop
    element-desktop

    # Development
    awscli2
    cypress
    difftastic
    docker-compose
    gcc
    gnuplot
    gotty
    gsh
    highlight
    hugo
    jq
    kubectl
    lens
    ngrok
    nodejs-16_13_1
    yarnWithNode16
    nodePackages.node2nix
    peek
    pick-colour-picker
    python3
    robo3t
    shellcheck
    pavucontrol
    haskell-language-server
  ];
}
