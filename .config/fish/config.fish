# FISH Shell config file

# GREETING - Currently none
set fish_greeting

# EDITOR
set -x EDITOR "emacs -q"

# PATH
set -x PATH '/home/colin/code/haskell/cabal/.cabal-sandbox/bin' '/bin' '/usr/local/bin' '/usr/bin'  '/usr/sbin' '/sbin' '/home/colin/.cabal/bin' '/home/colin/.gem/ruby/2.1.0/bin' '/opt/cuda/bin'

# FOR ADENDA DEV
set -x GOOGLE_APPLICATION_CREDENTIALS '/home/colin/code/AdendaServer/Source/AdendaServer/conf/adenda-server-0d72fe7d5d09.json'

# set -x CC 'clang'
set -x CC 'gcc'

set PATH (find $PATH -type d)

set -x PYTHONPATH '/home/colin/code/lib/pymodules/'

set -x _JAVA_AWT_WM_NONREPARENTING 1

#set -x _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true' #' -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

set -x JAVA_HOME '/usr/lib/jvm/default'

set -x ARCH_HASKELL 'Colin Woodbury <colingw@gmail.com>'

# BROWSER
set -x BROWSER "qutebrowser"

# RANDOM SHORTCUTS
function dbs
  echo "Starting Mongo daemon..."
  sudo systemctl start mongodb.service
  echo "Starting MySQL daemon..."
  sudo systemctl start mysqld.service
end

function lw
  wine "/home/colin/Desktop/LogicWorks 5/LogicWorks 5/LogicWorks.exe"
end

function hisp
  /home/colin/code/haskell/hisp/hisp $argv
end

function epc
  sudo emacs /etc/pacman.conf
end

function partners
  python2 /home/colin/code/python/teachingprogs/partners.py
end

function ha
  cd /home/colin/code/haskell/aura/
end

function sc
  scalac *.scala
end

function s
  sensors
end

function gt
  nvidia-settings -q gpucoretemp -t
end

function rlmt
  sudo rc.d stop laptop-mode
  sudo rc.d start laptop-mode
end

function c
  clear
  ls
end

function cla
  clear
  ls -a
end

function cll
  clear
  ls -l
end

function r
  sudo systemctl reboot
end

function p
  sudo systemctl poweroff
end

function hdoc
  qutebrowser /usr/share/doc/ghc/html/libraries/index.html &
end

function batt
  acpi
end

function ut
  tar -zxvf $argv
end

# Play a CD
function pcd
  mplayer cdda://
end

# Play a DVD
function pdvd
  mplayer -mouse-movements dvdnav:// -nocache
end

# Eject a CD / DVD
function ed
  eject /dev/sr0
end

# Pick
function pick
  python2 ~/code/python/pygamestuff/name-star/name-star.py $argv
end

# PROGRAM SHORTCUTS
function adder
  /home/colin/code/haskell/adder/adder $argv
end

function h
  ghc $argv -Wall
end

function hi
  ghci /home/colin/code/haskell/ColinPrelude/Colin.hs
end

function py
  python $argv
end

function p2
  python2 $argv
end

function e
  emacsclient -nw $argv
end

function se
  sudo emacsclient -nw $argv
end

function a
  alsamixer
end

function n
  ncmpcpp
end

function m
  makepkg $argv
end

function sx
  startx
end

function ic
  wicd-client -n
end

# DIRECTORY SHORTCUTS
function code
  cd ~/code
end

function share
  cd ~/share/colinsharing
end

function log
  cd ~/.wine-grock/drive_c/Program\ Files\ \(x86\)/GOG.com/Legend\ of\ Grimrock
end

function bas
  cd /usr/local/games/Bastion
end

# CUSTOM COMMANDS
function mkcd
  mkdir $argv
  cd $argv
end

function gd
  git diff $argv --color
end

function ga
  git commit -a $argv
end

function gb
  git branch $argv
end

function gc
  git checkout $argv
end

function gl
  git log --graph
end

function eo
  emacsclient -nw (ack -l $argv)
end

# REVAMPED COMMANDS
function grep
   command grep -s -n --colour $argv
end

function rm
  command rm -i $argv
end

# MOUNTING SHORTCUTS
function mu
  mount ~/mnt/usb
end

function umu
  umount ~/mnt/usb
end

function mb
  mount ~/mnt/buffaloflash
end

function umb
  umount ~/mnt/buffaloflash
end

function mmd
  mount ~/mnt/METROIDRIVE
end

function ummd
  umount ~/mnt/METROIDRIVE
end

function msd
  mount ~/mnt/sd
end

function umsd
  umount ~/mnt/sd
end

function mk
  mount ~/mnt/kindle
end

function umk
  umount ~/mnt/kindle
end

function msfu
  mount ~/mnt/sfusb
end

function umsfu
  umount ~/mnt/sfusb
end

function pm
  mount | column -t
end

function msfu
  mount ~/mnt/sfusb
end

function umsfu
  umount ~/mnt/sfusb
end
