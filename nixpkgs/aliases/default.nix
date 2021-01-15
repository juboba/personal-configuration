import ./git.nix // 
import ./docker.nix //
import ./docker-compose.nix //
{
  ag = ''ag --smart-case -U --pager="less -MIRFX"'';
  agrep = "alias | grep ";
  beep = "echo -e \"\a\"";
  clipit = "xclip -i -selection clipboard";
  ec = "emacsclient";
  hm = "home-manager";
  hs = "hess";
  l = "ls";
  la = "ls -a";
  ll = "ls -rtl";
  lla = "ls -rtla";
  mono = "cd ~/Projects/genially/mono";
  c = "clear";
  o = "rifle";
  n = "nnn";
  r = "ranger";
  pgrep = "pgrep -a";
  suspend = "systemctl suspend";
  tb = "nc termbin.com 9999";
  toSpanish = "trans :es -brief";
  trim = "sed -e 's/^[ \t]*//'";
  vi = "vim";
}
