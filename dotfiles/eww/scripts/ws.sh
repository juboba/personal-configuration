#! /usr/bin/env bash

workspaces(){
  local Workspaces
  Workspaces=$(expr "$(wmctrl -d | grep -v NSP | wc -l)" - 1)
  local focused
  focused=$(wmctrl -d | grep '*' | awk '{print $1}')

  for ((workspace=0; workspace <= Workspaces; workspace++))
  do 
    if [ $workspace == "$focused" ]; then
      local ws=$workspace
      local icon="  "
      local class="workspace-focused"
      local buf+="(eventbox :class \"$class\" :cursor \"pointer\" :onclick \"wmctrl -s $ws \" (label :class \"$class\" :text \"$icon\"))"

    elif [ -n "$(wmctrl -l | awk '{print $2}' | grep $workspace)" ]; then 
      local ws=$workspace
      local icon=" 󰻃 "
      local class="workspace-occupied"
      local buf+="(eventbox :cursor \"pointer\" :onclick \"wmctrl -s $ws \" (label :class \"$class\" :text \"$icon\"))"
    else 
      local ws=$workspace
      local icon="  "
      local class="workspace-empty"   
    fi
  done
  echo "(box :orientation \"h\" :class \"workspaces\" :halign \"center\" :valign \"center\" :vexpand \"true\" :hexpand \"true\" $buf)"
}
workspaces
xprop -spy -root _NET_CURRENT_DESKTOP | while read -r; do 
workspaces 
done


