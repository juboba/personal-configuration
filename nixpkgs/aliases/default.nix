import ./git.nix // 
import ./docker.nix //
import ./docker-compose.nix //
{
  beep = "echo -e \"\a\"";
  c = "clear";
  cat = "bat";
  clipit = "xclip -i -selection clipboard";
  ec = "emacsclient";
  hm = "home-manager";
  hs = "hess";
  ls = "exa";
  l = "ls --sort=modified --group-directories-first";
  ll = "l -l --git --color=always";
  lla = "ll -a";
  la = "l -a";
  man = "man --pager=\"sh -c 'col -bx | bat -l man -p'\"";
  o = "rifle";
  p="pwd | sed -e 's/^\\///' -e 's/\\// -> /g' | lolcat";
  pgrep = "pgrep -a";
  r = "ranger";
  suspend = "systemctl suspend";
  tb = "nc termbin.com 9999";
  toSpanish = "trans :es -brief";
  tree = "l --tree --color=always";
  gtree = "tree -l --git | less -r";
  trim = "sed -e 's/^[ \t]*//'";
  vi = "vim";
  gduck = "steam-run gitduck";
}
