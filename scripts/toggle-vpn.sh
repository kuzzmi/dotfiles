#!/bin/sh

if pgrep -x openvpn >/dev/null
then
    sudo pkill openvpn
    xset -led named "Scroll Lock"
else
    echo 16 | sudo pia-launch &
    xset led named "Scroll Lock"
fi
