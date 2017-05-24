#!/bin/bash

notify-send "New Mail" $1 -t 5000 &

notmuch new &
