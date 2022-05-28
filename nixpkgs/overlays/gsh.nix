self: super: {
  gsh = import (fetchGit {
    url = "git@github.com:Genially/gsh";
    ref = "refs/heads/replace_tmuxp_with_smug";
    rev = "079b74cdc07ba6a2f1bb6dba05a0d45ddef0b690";
  }) {};
}
