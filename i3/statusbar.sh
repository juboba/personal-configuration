PIPE=/tmp/volume

get_icon(){
    if [ $1 = '[on]' ]
    then
        echo -e "\u25cf"
        #echo "{on}"
    else
        echo -e "\u25cb"
        #echo "{off}"
    fi
}

get_mic_icon(){
    if [ $1 = '[on]' ]
    then
        echo -e "\u25ce"
    else
        echo -e "\u25cd"
    fi
}

show_volume(){
    volume=$(amixer get Master |tail -1 |cut -d" " -f7 | tr -d "[]")
    is_on=$(amixer get Master |tail -1 |cut -d" " -f8)
    is_on_mic=$(amixer get Capture |tail -1 |cut -d" " -f8)
    icon=$(get_icon $is_on)
    icon_mic=$(get_mic_icon $is_on_mic)
    echo -e "$icon $volume $icon_mic $(date +"[%R] %a %d/%m/%Y")"
}

key_listener() { # Update on keypress
    while true
    do
        if read command param <$PIPE; then
            case $command in
                mute)
                    amixer -D pulse sset Master toggle >> /dev/null
                    ;;
                micmute)
                    amixer -D pulse sset Capture toggle >> /dev/null
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
