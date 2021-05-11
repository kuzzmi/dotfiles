#!/bin/bash

key=631a45c77c56816f8f88f05df6b8c4e9

curl -s "https://api.openweathermap.org/data/2.5/weather?appid=$key&q=odessa&units=metric" | jq .main.temp > /tmp/weather
