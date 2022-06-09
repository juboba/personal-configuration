#!/usr/bin/env bash

#cf fuzzy cd =)
cf () {
  DIR=$(cd "${1:-.}"; fd --type d | fzf --prompt "Go to dir: ")
  [ -n "$DIR" ] && cd "${1:-.}/$DIR"
}

#vf fuzzy vim open =)
vf () {
  cd "${1:-.}"
  FILE=$(fzf -e --preview 'bat --color always {+1}' --prompt "Open with Vim: ")
  [ -n "$FILE" ] && vim "$FILE"
  [ -n $1 ] && cd -
}

#ef fuzzy emacs client open =)
ef () {
  cd "${1:-.}"
  FILE=$(fzf -e --preview 'bat --color always {+1}' --prompt "Open in Emacs: ")
  [ -n "$FILE" ] && emacsclient "$FILE"
  [ -n $1 ] && cd -
}

#of fuzzy open =)
of () {
  [ -n $1 ] && cd "${1:-.}";
  FILE=$(fzf -e  --prompt "Open: ")
  [ -n "$FILE" ] && rifle "$FILE"
  [ -n $1 ] && cd -
}

ag () {
  if [ -n "$1" ]
  then
    alias | grep $1 | sed -e "s/alias \(.*\)='\(.*\)'/\1\t\2/" | bat --tabs 0
  else
    alias | sed -e "s/alias \(.*\)='\(.*\)'/\1\t\2/" | bat --tabs 0
  fi
}

mkcd() {
  mkdir "$1" && cd "$_"
}

gitLinesForAuthor() {
  git log --author="$1" --pretty=tformat: --numstat | awk -F" " '{ added += $1; removed += $2 } END { print "added: ",  added, "removed:", removed }'
}
