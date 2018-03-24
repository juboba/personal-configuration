#!/usr/bin/env bash
AMIXER_CMD="amixer -D pulse"
AMIXER_INC=5%
REMOVE_BRACKETS="tr -d '[]%'"

vol_up() {
    read -r a b <<<$($AMIXER_CMD sset Master $AMIXER_INC+ | grep "^  Front Right" | cut -d' ' -f7,8 | $REMOVE_BRACKETS)
    echo $a $b
}

vol_down() {
    read -r a b <<<$($AMIXER_CMD sset Master $AMIXER_INC- | grep "^  Front Right" | cut -d' ' -f7,8 | $REMOVE_BRACKETS)
    echo $a $b
}

vol_toggle() {
    read -r a b <<<$($AMIXER_CMD sset Master toggle | grep "^  Front Right" | cut -d' ' -f7,8 | $REMOVE_BRACKETS)
    echo $a $b
}

vol_getCurrent() {
    read -r a b <<<$($AMIXER_CMD get Master | grep "^  Front Right" | cut -d' ' -f7,8 | $REMOVE_BRACKETS)
    echo $a $b
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
