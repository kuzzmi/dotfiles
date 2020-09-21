#!/bin/sh

if pgrep -x openvpn >/dev/null
then
  sudo pkill openvpn
  xset -led named "Scroll Lock"
else
  sudo openvpn --config /etc/openvpn/privateinternetaccess/DE\ Berlin.ovpn --config /etc/openvpn/privateinternetaccess/auth/auth.ovpn & > /dev/null
  xset led named "Scroll Lock"
fi
