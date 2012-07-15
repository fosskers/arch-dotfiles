# FISH Shell config file

# GREETING - Currently none
set fish_greeting

# EDITOR
set -x EDITOR "emacs -nw"

# PATH
set -x PATH '/bin' '/usr/bin' '/usr/local/bin' '/usr/sbin' '/sbin'

# RANDOM SHORTCUTS
function c
  clear
end

function cl
  clear
  ls
end

function r
  sudo /sbin/reboot
end

function p
  sudo /sbin/poweroff
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

function ed
  eject /dev/sr0
end

# PROGRAM SHORTCUTS
function h
  ghc $argv -Wall
end

function hi
  ghci /home/colin/code/haskell/ColinPrelude/ColinPrelude.hs
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
function grepc
  grep --colour $argv
end

function rm
  rm -i $argv
end

function gd
  git diff $argv --color
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

function pm
  mount | column -t
end
