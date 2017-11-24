#!/bin/sh
xrandr --output VIRTUAL1 --off --output eDP1 --off --output DP1 --off --output HDMI2 --off --output HDMI1 --off --output DP1-3 --off --output DP1-2 --primary --mode 3840x2160 --pos 0x0 --rotate normal --output DP1-1 --off --output DP2 --off
xset r rate 250 50
setxkbmap -model pc104 -layout us,ua,ru -variant colemak,winkeys,winkeys -option grp:shifts_toggle -option caps:escape
