{
  programs.nixvim = {
    options = {
      termguicolors = true;
      hlsearch = true;
      incsearch = true;

      number = true;
      relativenumber = true;
      numberwidth = 2;
      swapfile = false;
      cursorline = true;
      ruler = false;
      showcmd = true;
      showmode = false;

      splitbelow = true;
      splitright = true;
      undofile = true;
      undolevels = 10000;

      signcolumn = "yes";
      cmdheight = 1;
      wildmenu = true;

      foldlevel = 99;
      foldlevelstart = 99;
      foldenable = true;

      # Tab options
      tabstop = 2;
      shiftwidth = 2;
      softtabstop = 0;
      expandtab = true;
      autoindent = true;

      updatetime = 100;
      timeout = true;
      timeoutlen = 300;
    };
  };
}
