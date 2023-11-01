#!/usr/bin/env bash

volume-control
pactl subscribe | grep --line-buffered "sink"| while read -r; do volume-control; done;
