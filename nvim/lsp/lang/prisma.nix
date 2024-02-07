{ config, pkgs, ... }:

{
  programs.nixvim = {
    plugins = {
      lsp.servers.prismals.enable = true;

      treesitter = {
        grammarPackages = with config.programs.nixvim.plugins.treesitter.package.builtGrammars; [
          prisma
        ];
      };
    };
  };
}

