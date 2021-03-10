#cf fuzzy cd =)
cf () {
  DIR=$(fd --type d | fzf)

  [ -n "$DIR" ] && cd $DIR
}

#vf fuzzy vim open =)
vf () {
  FILE=$(fzf)
  [ -n "$FILE" ] && vim "$FILE"
}

#of fuzzy open =)
of () {
  rifle $(fzf)
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
