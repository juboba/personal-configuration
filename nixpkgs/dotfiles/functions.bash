#cf fuzzy cd =)
cf () {
  DIR=`rg --files \
  | sed -e "s/\(.*\)\/.*$/\1/" \
  | sort \
  | uniq \
  | fzy`

  echo $DIR
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
