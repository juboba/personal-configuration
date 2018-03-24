#!/usr/bin/env bash

STATUS=$(qdbus org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlaybackStatus)

ARTIST=$(qdbus org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Metadata | grep xesam:artist | cut -d':' -f3)

SONGNAME=$(qdbus org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Metadata | grep xesam:title | cut -d':' -f3)

STATUS_ICON=""

if [ $STATUS = "Playing" ]
then
    STATUS_ICON=""
fi

echo "$STATUS_ICON$ARTIST |$SONGNAME"
