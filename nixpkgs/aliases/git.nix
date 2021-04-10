# git aliases taken from bash-it
# TODO: Remove all the one I don't use...
{
  gcl = "git clone";
  ga = "git add";
  gap = "git add -p";
  gall = "git add -A";
  gf = "git fetch --all --prune";
  gft = "git fetch --all --prune --tags";
  gfv = "git fetch --all --prune --verbose";
  gftv = "git fetch --all --prune --tags --verbose";
  gus = "git reset HEAD";
  gpristine = "git reset --hard && git clean -dfx";
  gclean = "git clean -fd";
  gm = "git merge";
  gmv = "git mv";
  g = "git";
  get = "git";
  gs = "git status";
  gss = "git status -s";
  gsu = "git submodule update --init --recursive";
  gl = "git pull";
  gpl = "git pull";
  glum = "git pull upstream master";
  gpr = "git pull --rebase";
  gpp = "git pull && git push";
  gup = "git fetch && git rebase";
  gp = "git push";
  gpd = "git push --delete";
  gpo = "git push origin HEAD";
  gpu = "git push --set-upstream";
  gpuo = "git push --set-upstream origin";
  gpuoc = "git push --set-upstream origin $(git symbolic-ref --short HEAD)";
  gpom = "git push origin master";
  gr = "git restore";
  grs = "git restore --staged";
  gd = "git diff";
  gds = "git diff --staged";
  gdt = "git difftool";
  gdv = ''git diff -w "$@" | vim -R -'';
  gc = "git commit -v";
  gca = "git commit -v -a";
  gcm = "git commit -v -m";
  gcam = "git commit -v -am";
  gci = "git commit --interactive";
  gcamd = "git commit --amend";
  gb = "git branch";
  gba = "git branch -a";
  # FROM https://stackoverflow.com/a/58623139/10362396
  gbc = ''git for-each-ref --format="%(authorname) %09 %(if)%(HEAD)%(then)*%(else)%(refname:short)%(end) %09 %(creatordate)" refs/remotes/ --sort=authorname DESC'';
  gbt = "git branch --track";
  gbm = "git branch -m";
  gbd = "git branch -d";
  gbD = "git branch -D";
  gcount = "git shortlog -sn";
  gcp = "git cherry-pick";
  gcpx = "git cherry-pick -x";
  gco = "git checkout";
  gcom = "git checkout master";
  gcb = "git checkout -b";
  gcob = "git checkout -b";
  gct = "git checkout --track";
  gcpd = "git checkout master; git pull; git branch -D";
  gexport = "git archive --format zip --output";
  gdel = "git branch -D";
  gmu = "git fetch origin -v; git fetch upstream -v; git merge upstream/master";
  gll = "git log --graph --pretty=oneline --abbrev-commit";
  gg = "git log --graph --pretty=format:'%C(bold)%h%Creset%C(magenta)%d%Creset %s %C(yellow)<%an> %C(cyan)(%cr)%Creset' --abbrev-commit --date=relative";
  ggf = "git log --graph --date=short --pretty=format:'%C(auto)%h %Cgreen%an%Creset %Cblue%cd%Creset %C(auto)%d %s'";
  ggs = "gg --stat";
  gshw = "git show";
  gsl = "git shortlog -sn";
  gwc = "git whatchanged";
  gt = "git tag";
  gta = "git tag -a";
  gtd = "git tag -d";
  gtl = "git tag -l";
  gpatch = "git format-patch -1";
  # From http://blogs.atlassian.com/2014/10/advanced-git-aliases/
  # Show commits since last pull
  gnew = "git log HEAD@{1}..HEAD@{0}";
  # Add uncommitted and unstaged changes to the last commit
  gcaa = "git commit -a --amend -C HEAD";
  # Rebase with latest remote master
  gprom = "git fetch origin master && git rebase origin/master && git update-ref refs/heads/master origin/master";
  gpf = "git push --force";
  gpunch = "git push --force-with-lease";
  ggui = "git gui";
  gcsam = "git commit -S -am";
  # Stash aliases
  gst = "git stash";
  gstb = "git stash branch";
  gstd = "git stash drop";
  gstl = "git stash list";
  # Push introduced in git v2.13.2
  gstpu = "git stash push";
  gstpum = "git stash push -m";
  # Save deprecated since git v2.16.0
  # - aliases now resolve to push
  gsts = "git stash push";
  gstsm = "git stash push -m";
  # Alias gstpo added for symmetry with gstpu (push)
  # - gstp remains as for pop due to long-standing usage
  gstpo = "git stash pop";
  gstp = "git stash pop";
  # Switch aliases - Requires git v2.23+
  gsw = "git switch";
  gswm = "git switch master";
  gswc = "git switch --create";
  gswt = "git switch --track";
  # Git home
  ghm = ''cd "$(git rev-parse --show-toplevel)"'';
  # Show untracked files
  gu = "git ls-files . --exclude-standard --others";
  gtls = "git tag -l | sort -V";
}
