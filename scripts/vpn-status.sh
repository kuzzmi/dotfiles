if pgrep -x openvpn >/dev/null
then
  echo "<fc=#ffcc00>VPN</fc>"
else
  echo "VPN"
fi
