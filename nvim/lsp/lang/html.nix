{ config, ... }:

{
  programs.nixvim = {
    plugins = {
      lsp.servers.html = {
        enable = true;

        extraOptions.settings = {
          html = {
            format = {
              templating = true;
              wrapLineLength = 80;
              wrapAttributes = "auto";
            };
            hover = {
              documentation = true;
              references = true;
            };
          };
        };
      };

      lsp.servers.emmet_ls = {
        enable = true;

        filetypes = [
          "css"
          "html"
          "javascript"
          "javascriptreact"
          "typescript"
          "typescriptreact"
          "sass"
          "scss"
          "less"
        ];
      };

      treesitter = {
        grammarPackages = with config.programs.nixvim.plugins.treesitter.package.builtGrammars; [
          html
        ];
      };
    };
  };
}
