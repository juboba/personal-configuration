self: super: {
    gsh = import (fetchGit {
      url = "git@github.com:Genially/gsh";
      #ref = "master";
      #rev = "v0.3.0";
      #sha256 = "10xhblbyw8mvak58d294hbxxnf5sq0akj6qldv7brgm6944zppm0";
    });
  }
