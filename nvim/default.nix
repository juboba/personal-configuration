{ config, lib, pkgs, ... }:

{
  imports = [
    ./coding
    ./config
    ./editor
    ./lsp
  ];

  programs.nixvim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;

    luaLoader.enable = true;

    extraPlugins = with pkgs.vimPlugins; [ plenary-nvim ];
  };
}
