#!/usr/bin/env bash

#cf fuzzy cd =)
cf () {
  DIR=$(cd "${1:-.}"; fd --type d | fzf)
  [ -n "$DIR" ] && cd "${1:-.}/$DIR"
}

#vf fuzzy vim open =)
vf () {
  cd "${1:-.}"
  FILE=$(fzf -e --preview 'bat --color always {+1}')
  [ -n "$FILE" ] && vim "$FILE"
  [ -n $1 ] && cd -
}

#of fuzzy open =)
of () {
  [ -n $1 ] && cd "${1:-.}";
  rifle $(fzf)
  [ -n $1 ] && cd -
}

set_title () {
  PROMPT_COMMAND="echo -ne \"\033]0;$*\007\""
}

ag () {
  if [ -n "$1" ]
  then
    alias | grep $1 | sed -e "s/alias \(.*\)='\(.*\)'/\1\t\2/" | bat --tabs 0
  else
    alias | sed -e "s/alias \(.*\)='\(.*\)'/\1\t\2/" | bat --tabs 0
  fi
}
