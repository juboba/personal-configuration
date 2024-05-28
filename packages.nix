{ pkgs, ... }:

let
  node20_9_0 = (import
    (fetchTarball {
      name = "nixpkgs-node20_9_0";
      url = "https://github.com/NixOS/nixpkgs/archive/a71323f68d4377d12c04a5410e214495ec598d4c.tar.gz";
      sha256 = "1n6233gn8b66x252k8hvk5xygn6rxj2prnls2mh0d2hb8hnhypx6";
    })
    { }).nodejs_20;

  yarn = pkgs.yarn.overrideAttrs (oldAttrs: rec { buildInputs = [ node20_9_0 ]; });

  comma = with pkgs; callPackage
    (fetchFromGitHub {
      name = "comma";
      owner = "nix-community";
      repo = "comma";
      rev = "02e3e5545b0c62595a77f3d5de1223c536af0614";
      sha256 = "0qgg632ky6bnwkf9kh1z4f12dilkmdy0xgpal26g2vv416di04jq";
    })
    { inherit pkgs; };

in
with pkgs; {
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
    luaPackages.jsregexp
    juboba-bin
    nixfmt-classic
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
    #kotatogram-desktop
    element-desktop
    telegram-desktop

    # Development
    awscli2
    cargo
    cypress
    delta
    docker-compose
    emacs-lsp-booster
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
    node20_9_0
    yarn
    nodePackages.node2nix
    peek
    pick-colour-picker
    python3
    robo3t
    typescript
    shellcheck
    pavucontrol
    haskell-language-server

    # Security
    sops

    # lsp-bridge deps

    python311Packages.epc
    python311Packages.orjson
    python311Packages.sexpdata
    python311Packages.six
    python311Packages.setuptools
    python311Packages.paramiko
    python311Packages.rapidfuzz
  ];
}
