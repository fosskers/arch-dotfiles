# FISH Shell config file

# GREETING - Currently none
set fish_greeting

# EDITOR
set -x EDITOR "emacs -nw"

# RANDOM SHORTCUTS
function c
  clear
end

function cl
  clear
  ls
end

function r
  sudo reboot
end

function p
  sudo poweroff
end

function hdoc
  chromium /usr/share/doc/ghc/html/libraries/index.html &
end

# PROGRAM SHORTCUTS
function h
  ghc
end

function hi
  ghci
end

function py
  python $argv
end

function e
  emacs -nw $argv
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

# REVAMPED COMMANDS
function grep
  grep --colour $argv
end

function rm
  rm -i $argv
end

# MOUNTING SHORTCUTS
function mb
  mount ~/mnt/buffaloflash
end

function umb
  umount ~/mnt/buffaloflash
end

function pm
  mount | column -t
end
