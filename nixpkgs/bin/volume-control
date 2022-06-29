#!/usr/bin/env bash
AMIXER_SET_CMD="amixer sset Master"
AMIXER_GET_CMD="amixer get Master"
AMIXER_INC=5%
REMOVE_BRACKETS="tr -d '[]%'"

run_command() {
    echo "running..." $1 > /tmp/debug_log
    read -r a b <<<$($1 | grep "^  Front Right" | cut -d' ' -f7,8 | $REMOVE_BRACKETS)
    echo $a $b
}

vol_up() {
    run_command "$AMIXER_SET_CMD $AMIXER_INC+"
}

vol_down() {
    run_command "$AMIXER_SET_CMD $AMIXER_INC-"
}

vol_toggle() {
    run_command "$AMIXER_SET_CMD toggle"
}

vol_getCurrent() {
    run_command "$AMIXER_GET_CMD"
}

format_volume() {
    result=$($1)
    vol=$(echo $result | cut -d' ' -f1)
    is_on=$(echo $result | cut -d' ' -f2)

    if [ $is_on = "off" ]
    then
        icon=
    elif [ $vol -gt 30 ]
    then
        icon=
    else
        icon=
    fi

    echo $icon $vol%
}

if [ "$1" = "up" ]
then
	  echo $(format_volume vol_up)
elif [ "$1" = "down" ]
then
	  echo $(format_volume vol_down)
elif [ "$1" = "mute" ]
then
	  echo $(format_volume vol_toggle)
else
	  echo $(format_volume vol_getCurrent)
fi

# read -r a b <<<$(amixer -D pulse get Master | grep "^  Front Right" | cut -d' ' -f7,8 | sed -e 's/\[//g' -e 's/\]//g'); echo "$a $b"
