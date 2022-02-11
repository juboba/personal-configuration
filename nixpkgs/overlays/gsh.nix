self: super: {
    gsh = import (fetchGit {
      url = "git@github.com:Genially/gsh";
		  ref = "refs/heads/add_genially_cli";
      rev = "27280b7b819637cf8ba1eebedbcc9b07e2b6d9af";
    }) {};
  }
