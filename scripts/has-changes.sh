#!/bin/sh

cd ~/.config/dotfiles

CHANGES=$(git status -s | wc -l)

if [ $CHANGES -eq "0" ]; then
    echo ""
else
    echo "#[fg=colour0,bg=colour1,bold] * "
fi
