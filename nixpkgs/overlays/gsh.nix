self: super: {
    gsh = import (fetchGit {
      url = "git@github.com:Genially/gsh";
    }) {};
  }
