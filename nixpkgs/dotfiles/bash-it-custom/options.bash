# Use vim edition in readline
set -o vi

# `cd` when quit ranger
source ~/.config/ranger/shell_automatic_cd.sh

if [ -f ~/.config/nnn/plugins/quitcd.bash_zsh ]
then
  source ~/.config/nnn/plugins/quitcd.bash_zsh
fi

# Disable terminal suspension with Ctrl + s and Ctrl + q
[ -t 1 ] && stty -ixon -ixoff
