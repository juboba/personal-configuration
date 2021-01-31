import ./git.nix // 
import ./docker.nix //
import ./docker-compose.nix //
{
  ag = ''ag --smart-case -U --pager="less -MIRFX"'';
  agrep = "alias | grep ";
  beep = "echo -e \"\a\"";
  c = "clear";
  cat = "bat";
  clipit = "xclip -i -selection clipboard";
  ec = "emacsclient";
  hm = "home-manager";
  hs = "hess";
  l = "exa";
  la = "exa -a";
  ll = "exa -l --sort=created";
  lla = "exa -la --sort=created";
  ls = "exa";
  mono = "cd ~/Projects/genially/mono";
  n = "nnn";
  o = "rifle";
  pgrep = "pgrep -a";
  r = "ranger";
  suspend = "systemctl suspend";
  tb = "nc termbin.com 9999";
  toSpanish = "trans :es -brief";
  trim = "sed -e 's/^[ \t]*//'";
  vi = "vim";
}
