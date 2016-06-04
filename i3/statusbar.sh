PIPE=/tmp/volume

get_icon(){
    if [ $1 = '[on]' ]
    then
        #echo -e "\u25cf"
        echo "{on}"
    else
        #echo -e "\u25cb"
        echo "{off}"
    fi
}

show_volume(){
    volume=$(amixer get Master |tail -1 |cut -d" " -f6 | tr -d "[]")
    is_on=$(amixer get Master |tail -1 |cut -d" " -f8)
    icon=$(get_icon $is_on)
    echo -e "$icon $volume $(date +"[%R] %a %d/%m/%Y")"
}

key_listener() { # Update on keypress
    while true
    do
        if read command param <$PIPE; then
            case $command in
                mute)
                    amixer -D pulse sset Master toggle >> /dev/null
                    ;;
                vol)
                    if [[ $param == "-" ]]; then
                        amixer -D pulse sset Master 5%- >> /dev/null
                    fi
                    if [[ $param == "+" ]]; then
                        amixer -D pulse sset Master 5%+ >> /dev/null
                    fi
                ;;
            esac
        fi
        show_volume
    done
}

time_step() {    # Update every minute
    while true;
    do
        show_volume
        sleep 1m
    done
}

time_step &
key_listener &

wait
