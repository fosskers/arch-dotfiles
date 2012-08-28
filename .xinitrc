# UNIVERSAL IMPUT MANAGER - FOR JAPANESE
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &
export XMODIFIERS=@im='uim'

# MUSIC PLAYER DAEMON
mpd ~/.mpd.conf

# STUFF FOR XMONAD AND THE XSERVER
# hsetroot -fill /home/colin/backgrounds/rukia.jpg
setwall -r /home/colin/backgrounds/
xsetroot -cursor_name left_ptr
xmobar &
xmonad


