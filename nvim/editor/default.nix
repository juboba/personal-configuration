{ pkgs, ... }:
let
  dashboard = builtins.readFile ./_lua/dashboard.lua;
in
{
  programs.nixvim = {
    keymaps = [
      # neotree
      {
        mode = "n";
        key = "<leader>e";
        action = ":Neotree action=focus reveal toggle<CR>";
        options = {
          silent = true;
          desc = "File Explorer (root)";
        };
      }

      # spectre
      {
        action = "<cmd>lua require('spectre').open()<cr>";
        key = "<leader>sra";
        options = {
          desc = "In all files (spectre)";
        };
        mode = [ "n" ];
      }

      {
        action = "<cmd>lua require('spectre').open_visual({select_word=true})<cr>";
        key = "<leader>srw";
        options = {
          desc = "Current word (spectre)";
        };
        mode = [ "n" ];
      }

      {
        action = "<cmd>lua require('spectre').open_visual()<cr>";
        key = "<leader>srw";
        options = {
          desc = "Current word (spectre)";
        };
        mode = [
          "v"
        ];
      }

      {
        action = "<cmd>lua require('spectre').open_file_search({select_word=true})<cr>";
        key = "<leader>srb";
        options = {
          desc = "Current word in buffer (espectre)";
        };
        mode = [
          "n"
        ];
      }

      # project-nvim
      {
        action = "<cmd>Telescope projects<cr>";
        key = "<leader>fp";
        options = {
          desc = "Projects";
        };
        mode = [ "n" ];
      }

      # todo-comments
      {
        action = "<cmd>TodoTelescope<cr>";
        key = "<leader>st";
        options.desc = "Todos";
        mode = [ "n" ];
      }
      {
        action = "<cmd>TodoTelescope keywords=TODO,FIX,FIXME<cr>";
        key = "<leader>sT";
        options.desc = "Todo/Fix/Fixme";
        mode = [ "n" ];
      }

    ];

    # colorschemes.tokyonight.enable = true;
    colorschemes.catppuccin.enable = true;

    clipboard = {
      providers.xsel.enable = true;
      register = "unnamedplus";
    };

    extraPlugins = with pkgs.vimPlugins; [
      nvim-spectre
      nvim-web-devicons
      dashboard-nvim
    ];

    plugins = {
      fidget.enable = true;

      neo-tree = {
        enable = true;

        closeIfLastWindow = true;
        window = {
          width = 30;
          autoExpandWidth = true;
        };
      };

      project-nvim = {
        enable = true;

        manualMode = false;
      };

      todo-comments.enable = true;

      persistence.enable = true;

      which-key = {
        enable = true;

        plugins.spelling.enabled = true;

        registrations = {
          "<leader>f" =  "file";
          "<leader>s" =  "search";
          "<leader>sr" =  "replace";
          "<leader>w" =  "windows";
          "<leader>b" =  "buffers";
          "<leader>n" =  "notes";
          "]" = "+next";
          "[" = "+prev";
        };
      };
    };

    extraConfigLua = ''
      ${dashboard}
    '';
  };

  imports = [
    ./git.nix
    ./telescope.nix
    ./trouble.nix
    ./lualine.nix
  ];
}

