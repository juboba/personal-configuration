self: super: {
    gsh = import (fetchGit {
      url = "git@github.com:Genially/gsh";
		  ref = "refs/heads/main";
      rev = "c938d20b4fb334d6e6684006c079d519155f0c54";
    }) {};
  }
