self: super: {
  gsh = import (fetchGit {
    url = "git@github.com:Genially/gsh";
    ref = "refs/heads/main";
    rev = "6375b537c5f20ec12eaad8f138c6f897fb5bd4f3";
  }) {};
}
