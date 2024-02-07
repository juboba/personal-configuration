{ ... }:
let
  diagnosticsConfig = builtins.readFile ./diagnostics_config.lua;
  autocommands = builtins.readFile ./autocommands.lua;
  handlers = builtins.readFile ./handlers.lua;
  commands = builtins.readFile ./commands.lua;
in
{
  programs.nixvim = {
    extraConfigLuaPre = ''
      ${autocommands}
      ${commands}
      ${handlers}
    '';

    keymaps = [
      {
        action = "<cmd>lua vim.lsp.buf.code_action()<CR>";
        key = "<leader>ca";
        mode = [ "n" "v" ];
        options = {
          desc = "Code Actions";
        };
      }

      {
        action = "<cmd>Format<CR>";
        key = "<leader>cf";
        mode = [ "n" "v" ];
        options = {
          desc = "Conform Format";
        };
      }
    ];

    plugins = {
      which-key.registrations = {
        "<leader>c" =  "code";
        "g" =  "goto";
        "]" = "+next";
        "[" = "+prev";
      };

      lsp = {
        enable = true;

        keymaps = {
          silent = true; # don't show the keymaps messages when they are pressed
          diagnostic = {
            "]d" = "goto_next";
            "[d" = "goto_prev";
          };

          lspBuf = {
            K = "hover";
            gD = "declaration";
            gr = "references";
            gd = "definition";
            gi = "implementation";
            gt = "type_definition";
            "<leader>cr" = {
              action = "rename";
              desc = "Rename";
            };
          };
        };

        preConfig = ''${diagnosticsConfig} '';
      };

      conform-nvim = {
        enable = true;

        formatOnSave = {
          lspFallback = true;
          timeoutMs = 500;
        };
      };

      lint = {
        enable = true;
      };
    };

  };

  imports = [
    ./lang/typescript.nix
    ./lang/lua.nix
    ./lang/nix.nix
    ./lang/css.nix
    ./lang/html.nix
    ./lang/yaml.nix
    ./lang/json.nix
    ./lang/python.nix
    ./lang/prisma.nix
  ];
}
