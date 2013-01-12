# UNIVERSAL IMPUT MANAGER - FOR JAPANESE
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &
export XMODIFIERS=@im='uim'

# MUSIC PLAYER DAEMON
mpd ~/.mpd.conf

# ENLIGHTENMENT 17 - Added 2012 Dec 26
#exec enlightenment_start

# STUFF FOR XMONAD AND THE XSERVER
setwall -r /home/colin/backgrounds/
xsetroot -cursor_name left_ptr
xmobar &
xmonad


