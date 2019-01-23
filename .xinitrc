#!/bin/bash

export I3_CONFIG_DIR="$(dirname $(readlink -f $0))"

/usr/bin/xcalib -d :0 /usr/share/color/icc/xiaomi/6500.icm

start-pulseaudio-x11 &

# disks automounting
udiskie &



>> /dev/null which nm-applet && nm-applet &

i3-regenerate
exec i3

alias config='/usr/bin/git --git-dir=/home/larionov/.cfg/ --work-tree=/home/larionov

ulimit -n 8128

dex -a &

