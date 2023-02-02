let config-path = "~/repositories/personal-configuration";
in
import ./git.nix // 
import ./docker.nix //
import ./docker-compose.nix //
{
  beep = "echo -e \"\a\"";
  c = "clear";
  cat = "bat";
  clipit = "xclip -i -selection clipboard";
  ec = "emacsclient";
  gcal = "gcalcli calw $(date -d \"$D\" \"+%d\") 2 --monday";
  hm = "home-manager";
  ls = "exa --group-directories-first";
  l = "ls ";
  ll = "l -l --git --color=always";
  llt = "ll --sort=modified";
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
  vim = "nvim";
  vi = "vim";
  gduck = "steam-run gitduck";
  hmb = "home-manager build --impure --flake ${config-path}/nixos";
  hmd = "cd ${config-path}";
  hme = "vf ${config-path}";
  hms = "home-manager switch --impure --flake ${config-path}/nixos";
  nsn = "nix search nixpkgs";
}
