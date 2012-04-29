# NOTES
# acpi -> Reports battery power.
# cat /proc/meminfo -> System memory information
# cat /proc/cpuinfo -> CPU information
# scp -> Copy a file from one compy to another!
#        e.g. scp lol.txt colin@130.179.228.103:/home/ENG/colin
# sudo modprobe coretemp && sensors -> Display various temperatures.
# lsusb -> Lists all USB devices (internal and external)
# cpufreq-info -> See processor info.
# xfburn -> Disc burning.
# figlet -> Text to large ascii.
# Convert sjis to utf8 -> iconv -f sjis -t utf8 'filename'

##################
# RANDOM SHORTCUTS
##################
alias cl='clear && ls'
alias c='clear'
alias r='sudo reboot'
alias p='sudo poweroff'
alias m='sudo /etc/rc.d/mpd start'
alias hdoc='chromium /usr/share/doc/ghc/html/libraries/index.html'

# Remaining Battery Power
alias batt='acpi'

# GPU Temperature
alias gt='nvidia-settings -q gpucoretemp -t'

# Default untarring
alias ut='tar -zxvf'

# Dad's CentOS box.
alias dco='ssh -C 130.179.228.103'  

# Turning the wireless interface on and off.
alias ro='sudo ifconfig wlan0 down'
alias ron='sudo ifconfig wlan0 up'

# Restart Laptop Mode Tools
alias rlmt='sudo rc.d stop laptop-mode && sudo rc.d start laptop-mode'

# Eject a CD / DVD
alias ed='eject /dev/sr0'  

# Play a DVD with menu and mouse movements enabled
alias pdvd='mplayer -mouse-movements dvdnav://'

# Play a CD
alias pcd='mplayer cdda://'

# Make a directory then go to it.
function mkcd() {
    mkdir -p "$1" && cd "$1"
}

# Copy a file to Dad's Cent OS box
function cdco() {
    scp "$1" colin@130.179.228.103:/home/ENG/colin
}

# Wine Shortcut
function wine32() {
    env WINEARCH=win32 WINEPREFIX=~/.wine32 "$1" "$2"
}

# Manually setting CPU frequency
function powerup() {
    for i in 0 1; do sudo cpufreq-set -c $i -g performance; done
}

function powerdown() {
    for i in 0 1; do sudo cpufreq-set -c $i -g ondemand; done
}

###################
# PROGRAM SHORTCUTS
###################
alias h='ghc'
alias hi='ghci ~/code/haskell/ColinPrelude/ColinPrelude.hs'
alias rl='~/code/haskell/running-racket/rl'
alias py='python'
alias p2='python2'
alias e='emacs -nw'
alias se='sudo emacs -nw'
alias a='alsamixer'
alias n='ncmpcpp'
alias sx='startx'
alias ic='wicd-client -n'
alias adder='~/code/haskell/adder/adder'
alias pick='python2 ~/code/python/pygamestuff/name-star/name-star.py'
alias rmexecs='/home/colin/code/haskell/rmexecs/rmexecs'

#####################
# DIRECTORY SHORTCUTS
#####################
alias code='cd ~/code'
alias share='cd ~/share/colinsharing'
alias sr='cd ~/.wine32/drive_c/Program\ Files/Steam/steamapps/common/skyrim'
alias pl='cd ~/code/lib/pymodules'
alias sto='cd ~/.wine32/drive_c/Program\ Files/Cryptic\ Studios/'

###################
# REVAMPED COMMANDS
###################
alias gcc='gcc -Wall'
alias grep='grep --colour'
alias rm='rm -i'
alias ls='ls -h --color=auto --group-directories-first'

####################
# MOUNTING SHORTCUTS
####################
alias mf='mount ~/mnt/flash'
alias umf='umount ~/mnt/flash'
alias mmd='mount ~/mnt/METROIDRIVE'
alias ummd='umount ~/mnt/METROIDRIVE'
alias mk='mount ~/mnt/kindle'
alias umk='umount ~/mnt/kindle'
alias mb='mount ~/mnt/buffaloflash'
alias umb='umount ~/mnt/buffaloflash'
alias pm='mount | column -t'

# Some Colours
txtcyn='\e[0;36m' # Cyan
bldblu='\e[1;34m' # Bold Blue
bldylw='\e[1;33m' # Bold Yellow
txtrst='\e[0m'    # Text Reset

# Bash Prompt - Looks messy but works!
echo -e "${bldblu}colin${txtrst} ${bldylw}::${txtrst} [${txtcyn}Host${txtrst}] ${bldylw}-> ${txtcyn}Dir ${bldylw}-> ${txtcyn}IO${txtrst} ()"
PS1="\[$bldblu\]\u \[$txtrst\](\h\[$bldylw\]:\[$txtcyn\]_\[$txtrst\]) \W = \[$txtcyn\]do \[$txtrst\]"

# Python libs.
export PYTHONPATH=${PYTHONPATH}:/home/colin/code/lib/pymodules/
export EDITOR='emacs -nw'