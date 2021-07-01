#!/bin/sh

STATE=`cat ~/.local/.office-ac`
ON="<fc=#99cc99>ON</fc>"
OFF="<fc=#838383>OFF</fc>"

if [[ $STATE == "$ON" ]]; then
  mosquitto_pub -h 192.168.88.132 -t 'pc/conditioner' -m 'off'
  echo $OFF > ~/.local/.office-ac
else
  mosquitto_pub -h 192.168.88.132 -t 'pc/conditioner' -m 'on'
  echo $ON > ~/.local/.office-ac
fi
