# UNIVERSAL IMPUT MANAGER - FOR JAPANESE
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &
export XMODIFIERS=@im='uim'

# compose key
setxkbmap -option compose:ralt

# MUSIC PLAYER DAEMON
mpd ~/.mpd.conf

# STUFF FOR XMONAD AND THE XSERVER
setwall -r /home/colin/backgrounds/
xsetroot -cursor_name left_ptr
#xmobar &
#xmonad
#exec dbus-launch xmonad
xmonad
