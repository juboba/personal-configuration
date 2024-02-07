{ config, pkgs, ... }:

{
  programs.nixvim = {

    plugins = {
      lsp.servers.ruff-lsp = {
        enable = true;
      };

      conform-nvim = {
        formattersByFt = {
          python = [ "black" ];
        };

        formatters = {
          black = {
            command = "${pkgs.black}/bin/black";
          };
        };
      };

      lint = {
        lintersByFt = {
          python = [ "ruff" ];
        };

        linters = {
          ruff = {
            cmd = "${pkgs.ruff}/bin/ruff";
          };
        };
      };

      treesitter = {
        grammarPackages = with config.programs.nixvim.plugins.treesitter.package.builtGrammars; [
          python
        ];
      };
    };
  };
}


