{ config, ... }: {
  programs.nixvim = {
    plugins = {
      treesitter = {
        enable = true;

        nixvimInjections = true;

        folding = true;
        indent = true;

        grammarPackages = with config.programs.nixvim.plugins.treesitter.package.builtGrammars; [
          c
          vim
          vimdoc
          markdown
          markdown_inline
          query
        ];
      };

      treesitter-refactor = {
        enable = true;

        highlightDefinitions.enable = true;
      };

      treesitter-context = {
        enable = true;

        maxLines = 5;
      };

      treesitter-textobjects = {
        enable = true;

        select = {
          enable = true;

          keymaps = {
            "aa" = "@parameter.outer";
            "ia" = "@parameter.inner";
            "af" = "@function.outer";
            "if" = "@function.inner";
            "ac" = "@class.outer";
            "ic" = "@class.inner";
            "ai" = "@conditional.outer";
            "ii" = "@conditional.inner";
            "al" = "@loop.outer";
            "il" = "@loop.inner";
            "ak" = "@block.outer";
            "ik" = "@block.inner";
            "is" = "@statement.inner";
            "as" = "@statement.outer";
            "ad" = "@comment.outer";
            "am" = "@call.outer";
            "im" = "@call.inner";
          };
        };

        move = {
          enable = true;

          setJumps = true;
          gotoNextStart = {
            "]m" = "@function.outer";
            "]]" = "@class.outer";
          };
          gotoNextEnd = {
            "]M" = "@function.outer";
            "][" = "@class.outer";
          };
          gotoPreviousStart = {
            "[m" = "@function.outer";
            "[[" = "@class.outer";
          };
          gotoPreviousEnd = {
            "[M" = "@function.outer";
            "[]" = "@class.outer";
          };
        };

        swap = {
          enable = true;

          swapNext = {
            ")a" = "@parameter.inner";
          };
          swapPrevious = {
            ")A" = "@parameter.inner";
          };
        };
      };
    };

    extraConfigLua =
      # lua
      ''
        local ts_repeat_move = require("nvim-treesitter.textobjects.repeatable_move")

        vim.treesitter.language.register("markdown", "mdx")

        -- vim way: ; goes to the direction you were moving.
        vim.keymap.set({ "n", "x", "o" }, ";", ts_repeat_move.repeat_last_move)
        vim.keymap.set({ "n", "x", "o" }, ",", ts_repeat_move.repeat_last_move_opposite)

        -- Optionally, make builtin f, F, t, T also repeatable with ; and ,
        vim.keymap.set({ "n", "x", "o" }, "f", ts_repeat_move.builtin_f)
        vim.keymap.set({ "n", "x", "o" }, "F", ts_repeat_move.builtin_F)
        vim.keymap.set({ "n", "x", "o" }, "t", ts_repeat_move.builtin_t)
        vim.keymap.set({ "n", "x", "o" }, "T", ts_repeat_move.builtin_T)
      '';
  };
}
