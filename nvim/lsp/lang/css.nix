{ config, pkgs, ... }:

{
  programs.nixvim = {
    plugins = {
      lsp.servers.cssls = {
        enable = true;
      };

      conform-nvim = {
        formattersByFt = {
          css = [ "prettierd" ];
        };

        formatters = {
          prettierd = {
            command = "${pkgs.prettierd}/bin/prettierd";
          };
        };
      };

      lsp.servers.tailwindcss = {
        enable = true;
      };

      lint = {
        lintersByFt = {
          css = [ "stylelint" ];
        };

        linters = {
          stylelint = {
            cmd = "${pkgs.stylelint}/bin/stylelint";
          };
        };
      };

      treesitter = {
        grammarPackages = with config.programs.nixvim.plugins.treesitter.package.builtGrammars; [
          css
        ];
      };
    };
  };
}
