dotfiles
========

This repo contains (or will contain eventually) all the configuration I use.

### Email

Email configuration is almost identical to one represented
in this [article](http://stevelosh.com/blog/2012/10/the-homely-mutt/).

It consists of:
- [offlineimap](http://www.offlineimap.org)
- [mutt](http://www.mutt.org/) (NeoMutt to be very precise)
- [msmtp](http://msmtp.sourceforge.net/)
- [notmuch](https://notmuchmail.org/)

Both offlineimap and msmtp use [GNOME/Keyring](https://wiki.archlinux.org/index.php/GNOME/Keyring)
for storing passwords.

On DE startup (in my case this is i3) this [workaround](https://mail.gnome.org/archives/gnome-keyring-list/2012-December/msg00000.html)
applied to get GNOME/Keyring working in cron.

Notifications are sent when there is a new unread email. 

### Window Manager

WM of choice is [i3wm](https://i3wm.org/).

### Media

Music is backed by [Mopidy](https://www.mopidy.com/) and [Mopidy-Spotify](https://github.com/mopidy/mopidy-spotify)

### Fonts

Font for a terminal: [Input Condensed](http://input.fontbureau.com/download/index.html?size=14&language=python&theme=solarized-dark&family=InputMono&width=200&weight=400&line-height=1.4&a=ss&g=0&i=serifs_round&l=serifs_round&zero=0&asterisk=height&braces=0&preset=default&customize=please)

### Software
- xclip
- polybar
- calcurse
- vivaldi
- rtorrent
- screen
- rofi
- mutt
- offlineimap

### Configuration

#### Keyboard

##### Layout
`.xorg.conf.d` contains `00-keyboard.conf`, which is responsible for setting the keyboard layout.
`.xmodmap` contains remapping of `Caps Lock` to `Esc`.

##### Keypress autorepeat
`.xinitrc` is configured at values `250 50`

#### Software:
- vim
- i3
- polybar + binaries

### Themes

[Arc Darker](https://github.com/horst3180/Arc-theme#installation) + [Arc Icons](https://github.com/horst3180/arc-icon-theme)
