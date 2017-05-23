dotfiles
========

This repo contains (or will contain eventually) all the configuration I use.

### Email

Email configuration is almost identical to one represented
in this [article](http://stevelosh.com/blog/2012/10/the-homely-mutt/).

It consists of:
- [mutt](http://www.mutt.org/) (NeoMutt to be very precise)
- [msmtp](http://msmtp.sourceforge.net/)
- [offlineimap](http://www.offlineimap.org)

Both offlineimap and msmtp use [GNOME/Keyring](https://wiki.archlinux.org/index.php/GNOME/Keyring)
for storing passwords.

Crontab for offlineimap looks like this:

```
* * * * * /home/kuzzmi/.local/bin/run-offlineimap.sh
```

On DE startup (in my case this is i3) this [workaround](https://mail.gnome.org/archives/gnome-keyring-list/2012-December/msg00000.html)
applied to get GNOME/Keyring working in cron.

### Window Manager

I prefer tiling window managers, so my WM of choice is [i3wm](https://i3wm.org/).
Particularly for this setup I use `i3-manjaro` group.

By the time of writing this, the group contains the following packages
that are required:

```
community/artwork-i3 20170508-1 (i3-manjaro)
community/conky-i3 20160915-1 (i3-manjaro)
community/dmenu-manjaro 4.6-12 (i3-manjaro)
community/i3-default-artwork 20170225-1 (i3-manjaro)
community/i3-gaps 4.13-1 (i3-manjaro)
community/i3-help 20160827-1 (i3-manjaro)
community/i3-scripts 20161003-1
community/i3-scrot 1.1-1
community/i3exit 20160915-1 (i3-manjaro)
community/i3lock 2.8-1 (i3)
community/i3status-manjaro 2.11-1 (i3-manjaro)
```

Required for this setup `dmenu-manjaro`, `i3-gaps` and `i3status-manjaro`
packages contain patches for additional functionalily.
