Log of Various Fixes
--------------------
2016 Jan 27 - Got `rime` working. Turned out I was missing Chinese fonts.
              After restarting the ibus daemon with:
                `ibus-daemon -drx`
              everything works. I've set `rime` up to interpret the Yale
              romanization scheme.

2016 Jan 19 - Switched to `ibus` from `uim`. Japanese input works in
              qutebrowser now. Changed input switching from Super+i
              to C+M+i.
              For some reason, `rime` (Canto input) doesn't work right yet.
              I can get the mode launched, but it refuses to type anything
              but roman characters, and I can't seem to change submodes
              within Rime.

2015 Oct 15 - Filesystem broke somehow. When GRUB attempted to load stage 2,
              it dumped instead to the GRUB shell.
              Used a live USB to boot in, and fixed everything with:
              `fsck.ext4 -y -f /dev/sda3`

2015 Sep 24 - An `ncurses` update broke everything that depended on it,
              including `fish`. This meant I couldn't open any programs,
              or even log in once I had restarted.
              I used a live Arch USB (hold down that C button for a long time)
              to manually mount my Arch, then I updated `ncurses`,
              `lib32-ncurses` and `fish`.

2015 May 03 - Switched to Colemak keyboard.
              Check settings: `localectl status`
              Change VC: `localectl set-keymap --no-convert colemak`
              Change X11: `localectl set-x11-keymap --no-convert us pc104 colemak`

2014 Dec 17 - Added `haveged` to produce entropy during key generation.
              Also nuke-n-paved old pacman keyring.
              Had to manually reregister the keys for haskell-core
              and the infinality fonts.

2014 Nov 15 - It seems like grub-legacy doesn't load multiple images,
              hence the kernel panics. I've removed the offending
              entry from my `menu.lst` until there's a solution.

2014 Nov 15 - Going to sit forever on 340xx branch of nvidia drivers.
              Replaced the stock `nvidia` packages with these:
              - nvidia-340xx
              - nvidia-340xx-utils
              - nvidia-340xx-libgl
              - lib32-nvidia-340xx-utils
              - lib32-nvidia-340xx-libgl
              And now my Xserver is alive again.

2014 Nov 14 - Big update. Had to do some manual magic to get java
              installed properly. X server is dead. `ucodes` problem?

2014 Jul 03 - Created `/etc/pm/power.d/laptop-mode` as a dummy hook
              to try and stop conflicts between laptop-mode-tools and
              pm-utils. My mouse was freezing a lot, even on AC.

2014 Jul 02 - Packages won't install. Tried:
                pacman-key --refresh-keys
              Then I deleted everything out of the package cache, and
              attempted a redownload. Seems to have worked.

2014 May 21 - All fonts are beautiful now thanks to the [infinality-bundle]
              repo and packages. Fonts are great out of the box. I didn't
              do any extra configuration.

2014 May 12 - Fixed non-responsive trackpad by rebuilding:
              `xf86-input-mtrack-git`
	      Enough kernel upgrades without rebuilding it probably killed it
	      after a while.

2014 Apr 30 - Figured out syncing music to Android.
     	      Installed `jmtpfs` and used `jmtpfs ~/mnt/mtp` to mount my phone
	      to the computer. Then I just copy over files as needed.
	      Use `fusermount -u ~/mnt/mtp` to unmount.
	      Note: If the screen locks at any point, the
              mount seems to revert to read-only status.
              You need to remount if this happens. One
              solution is to set the phone's screen lock time to
              a long time.

2013 Nov 17 - Got rid of ldconfig error messages by deleting:
     	      /usr/lib32/libnvcuvid.so.319.23
	      /usr/lib32/libnvidia-ml.so.319.23
	      /usr/lib32/libnvidia-tls.so.319.23
	      /usr/lib32/libnvidia-glcore.so.319.23
	      /usr/lib32/libnvidia-cfg.so.319.23
 	      /usr/lib32/libcuda.so.319.23
	      /usr/lib32/libGL.so.319.23
	      Shouldn't be problem, as they belong to no package. Or rather,
	      they belonged to an old version of nvidia that I've long since
	      upgraded from (currently at 331.20 with 3.12 kernel).

2013 Oct 24 - Added SFU arch mirror to mirrorlist.

2013 Oct ?? - Added wicd-eduroam and set up eduroam connection at SFU.

2013 Sep 21 - xmonad was giving a `executeFile: permission denied` error,
     	      which after recompiling xmonad seemed to fix things.
	      `xmonad --recompile` was the command.

2013 Jul 01 - Fixed broken setup with latest Arch live cd.
     	      The kernel and my nvidia packages weren't happy.

2013 Mar 24 - [haskell] switched to [haskell-core]
            - Switched to dzen2/conky.

2013 Feb 05 - Upgraded to ghc 7.6.2, had to reinstall all my haskell stuff.
     	      X11 being funny...?

2013 Jan 30 - Added `F1` as an IM switcher in UIM.

2013 Jan 20 - Added visualizations to ncmpcpp by adding entries to:
     	      ~/.ncmpcpp/config  and  ~/.mpd.conf

2012 Nov 03 - Fixed exploded system.
     	      Have added `modprobe.black=pata_acpi` to kernel command line.

2012 Oct 10 - Wiped and reinstalled ghc. Version 7.6 was causing some
     	      funky installation errors. Building aura was also failing.

2012 Sep 15 - Replaced initscripts with systemd.
     	      Suspend the system with `systemctl suspend`.

2012 Aug 31 - INTERNET FINALLY FIXED!!! Using broadcom-wl now.

2012 Aug 29 - Edited `/etc/acpi/handler.sh` slightly.
     	      In theory, laptop mode should stop and start depending on
	      whether a power cord is connected.
	    - Reduced package cache to 3 of each package.

2012 Aug 28 - Added Dropbox.
     	    - Added xmobar. Configuration can be found at ~/.xmobarrc

2012 Aug 25 - `cpufrequtils` has been depreciated. Installed `cpupower`
     	      in its place.

2012 Jul 15 - Internet still fucked.
     	    - /lib successfully symlinked to /usr/lib.
     	    - Fixed locale settings. I was missing en_US, and that was
	      causing problems. 

2012 Jul 10 - SUCCESSFULLY upgraded `b43-firmware`. No problems.
     	    - SUCCESSFULLY upgraded the kernel and nvidia drivers!
	      Kernel now at 3.4.4-2, nvidia at 302.17-1.
	    - SUCCESSFULLY upgrade `kmod`. Now at 9-1.
	    - Tested nvidia by playing Bastion a bit... seems quicker.

2012 Jun 25 - Added /usr/sbin to PATH.
     	    - One of the following blows up my system when upgraded:
	      -- e2fsprogs     (OK)
	      -- filesystem    (OK)
	      -- kmod          (NOT OKAY)
	      -- systemd-tools (OK)
            - `kmod 9-1` determined as the update that is exploding.
	      It doesn't like nvidia. On booting an xserver it says it can't
	      find a screen. Something about not being able to load the
	      device `nvidia`.
	      `kmod` upgrades will now be ignored by pacman.

2012 Jun 06 - Changed shell to `fish`. Config file is located at:
     	      ~/.config/fish/config.fish

2012 May 19 - /etc/acpi/handler.sh got a small updated (*.pacnew)
     	      but after reviewing it I decided to ignore it.

2012 May 10 - Tried to upgrade to the 3.3.5 kernel, but was betrayed
     	      once again by internet speeds. It looks like I will have
	      to be kernel-conservative for the foreseeable future.

2012 May 05 - xmonad borders will now be a random colour every time.

2012 Apr 29 - Got the fastest repo mirrors relative to my location by using
     	      `reflector`. 

2012 Apr 21 - Internet problems have most likely been fixed. I downgraded
     	      the kernel and the nvidia drivers using `pacman -U`.
	      Note that I found the package files in:
	        /var/cache/pacman/pkg/
	      The packages `linux` `nvidia` and `nvidia-utils` will also
	      be ignored upon uses of `-Syu`. This was set in pacman.conf.

2012 Apr 14 - Removed .Xresources as it was redundant.
     	    - Changed urxvt font and fixed the bold crappy Japanese problem
	      by supplying a second font in the URxvt.font field.
	      This provided a specific fallback for when a character
	      (something in Japanese) wasn't present in the main font.
	      Before it was defaulting to whatever it could find, then
	      bolding it according to the settings of the main font.
	    - Fixed ALSA hardware thing again with:
	      sudo alsactl -f /var/lib/alsa/asound.state store

2012 Apr 05 - Reformatted and setup a new 8gig USB stick.
     	      It can be mounted and unmounted with `mb` and `umb`.

2012 Mar 24 - Added `fsck` to the HOOKS section of mkinitcpio.conf
     	      This was added to the newer version of the conf file
	      after an update, so I added it manually while _leaving out_
	      `autodetect` because of what I remember of it causing trouble
	      during my initial Arch installation.

2012 Mar 14 - Fixed the weird speaker "beats" (pocks? sounds?) that would
     	      occur when opening the lid and de-sleeping. I made a change
	      in /etc/acpi/handler.sh.
	      Line 60, what was `then pm-suspend` is now wrapped in commands
	      that use `amixer` to mute and unmute alsa before and after sleeping.
	      I suppose the forced unmuting could be considered a bug, if alsa
	      was already muted beforehand and that was in fact prefered.

2012 Mar 0? - Fixed the weird error at boot-time having to do with ALSA
     	      and something hardware related. Resaving the ALSA levels
	      with a one-liner made the problem go away. Perhaps the old
	      settings were out-dated?

2012 Feb 21 - GTK2 themes are now bitchin'. This affects chromium, wicd-client,
     	      libreoffice, and graveman (among others).
	    - I can now burn CDs and DVDs with graveman.
	    - Tilem now works after I obtained a TI-83plus ROM.
	      This doesn't bother me, as I own one of the things.

2012 Feb 20 - Hmm... Hopefully the wireless is fixed. I applied two
     	      solutions:
	      1. pm-utils might have been too zealous. I added a script
	      	 that turns off wireless powersaving.
              2. wicd now uses dhclient and not dhcpcd to obtain IP
	      	 addressed. The connection process was always failing
		 at that part.

2012 Feb 16 - Something is fucked... internet only connects half the time.
     	    - Also, turns out I haven't updated [multilib] in forever!
	      Somehow it was disabled in pacman.conf...

2012 Feb 14 - Valentine's Day. Hacked some Haskell together to make Racket
     	      run interactively with multiple files loaded at once.
	      Running "rl file1.scm file2.scm file3.scm" translates to:
	      "racket -i -f file1.scm -f file2.scm -f file3.scm"

2012 Feb 08 - Sexified PS1 to make it look like a fully coloured Haskell
     	      function definition.

2012 Jan 30 - Got Wacom tablet working. (It worked fine plug-and-play)
     	    - This laptop will now sleep properly when I close the lid.

2012 Jan 26 - Made urxvt transparent by adding two lines to ~/.Xdefaults
     	    - `coretemp' will be loaded as a module now, and `sensors'
	      now prints CPU temperatures in a way that makes sense.
	      Perhaps kernel updates are to thank? :)

2012 Jan 25 - Got spellcheck working in Libreoffice by installing
     	      `hunspell' and `hunspell-en'
	    - Fixed the low DVD volume in mplayer by adding two values to
	      ~/.mplayer/config
	    - Succeeded in mounting an SD card.
	        sudo mount -t vfat <device-path> <mount-path>
	    - Kindle can now be mounted with `mk' and unmounted with 'umk'
	      Calibre can now see the device as well.

2012 Jan 18 - Fixed pacman (now version 4) by switching the .conf files around.
     	      Turns out the old .conf wouldn't work. Duh.

2012 Jan 09 - Updated rc.conf to the new layout.
     	      Old file is saved as rc.conf.old
	    - Computer unrelated: Pimped out my bike with technology :)

2012 Jan 06 - Laptop Mode Tools:
     	    - Keyboard and Mouse should not auto-suspend.
	    - Added `commit=600' to my HDs mount options in fstab.
	      This will reduce journaling period to once every ten minutes.
	    - Turns out my CPU was on the max frequency _all the time_.
	    - Added cpufreqtools, and have set my default frequency governor to
	      `ondemand'. Adding cpufreq to my DAEMONS array will ensure this
	      will be set on boot.
	      The custom commands `powerup' and `powerdown' can be used to 
	      switch between the `performance' and `ondemand' governors.

2012 Jan 05 - Got wicd working. I now have wireless internet!
     	    - Made various changes to xmonad.hs
	      Auto-shifting of programs to workspaces DOES NOT work.
	      Removed borders for Workspace 9 DOES.
	      For playing Skyrim, make sure to:
	      1. Move the system tray icon to another workspace.
	      2. De-tile the Skyrim window (command-t).

2011 Dec 27 - Fixed mpd by making all settings local to my username. 
     	      The daemon is now configured via /home/colin/.mpd.conf
	      It's likely that some some file permissions broke somehow.
	      mpd was also removed from the list of start-up daemons.

2011 Dec ?? - Got Skyrim working in a 32bit wine environment :)

2011 Dec 05 - Removed duplicates in my music directory.

2011 Dec 04 - Got Steam working. Got Portal working by installing lib32-nvidia-utils.
     	      This also made me able to play Diablo II fullscreen.

2011 Nov 28 - Switched to using `hsetroot' for setting the background image.

2011 Nov 27 - Fixed Japanese. Turned out commenting-out all the lines in .xinitrc
     	      that had to do with `uim' was the problem. I was not commenting-out
	      what I thought I was.

2011 Nov 23 - Function keys now work as they should.
     	    - Arch can at least _detect_ the wireless network.
	      Haven't actually connected yet.
	    - mpd is now loaded on boot. Manual starting was getting tedious.

2011 Nov 15 - GRUB was fucked and Arch wouldn't boot.
     	      Rewrote the MBR in OSX. This got rid of the extra Tux logo, 
	      and after re-installing GRUB, Arch would boot again. Yay!

2011 Nov 13 - mpd now fully working.
     	    - ncmpcpp installed and working!

2011 Nov 05 - Got mplayer working. No GUI is being used.

2011 Nov 03 - Added the option iocharset=utf8 to fstab, which allowed me to view
     	      Japanese file names on my USB drive!

2011 Nov 01 - My flashdrive can now be mounted freely with 'mf'.
     	    - METROIDRIVE can be mounted with 'mmd'.
     	    - Japanese broke for a while. Turned out locales were the problem.
	      Running locale-gen regenerated them, and upon a reboot, everything worked.