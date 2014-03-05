# FISH Shell config file

# GREETING - Currently none
set fish_greeting

# EDITOR
set -x EDITOR "emacs"

# PATH
set -x PATH '/bin' '/usr/bin' '/usr/local/bin' '/usr/sbin' '/sbin' '/home/colin/.cabal/bin'

set PATH (find $PATH -type d)

set -x PYTHONPATH '/home/colin/code/lib/pymodules/'

# BROWSER
set -x BROWSER "chromium"

# RANDOM SHORTCUTS
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
end

function cl
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
  chromium /usr/share/doc/ghc/html/libraries/index.html &
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

function e
  emacs -nw $argv
end

function se
  sudo emacs -nw $argv
end

function a
  alsamixer
end

function n
  ncmpcpp
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

# REVAMPED COMMANDS

function grep
   command grep -s -n --colour $argv
end

function rm
  command rm -i $argv
end

function gd
  git diff $argv --color
end

function ga
  git commit -a $argv
end

# MOUNTING SHORTCUTS
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
