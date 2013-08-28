{-# LANGUAGE LambdaCase #-}

import XMonad

-- LAYOUTS
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen 
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Circle
import XMonad.Layout.Spiral
import XMonad.Actions.GridSelect

-- WINDOW RULES
import XMonad.ManageHook

-- KEYBOARD & MOUSE CONFIG
import XMonad.Util.EZConfig
import XMonad.Actions.FloatKeys
import Graphics.X11.ExtraTypes.XF86

-- STATUS BAR
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Dmenu

--import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import System.IO (hPutStrLn)

--import XMonad.Operations
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS  -- nextWS, prevWS
import Data.List  -- clickable workspaces

import System.Exit (exitSuccess)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

---

myLayout = tiled ||| Mirror tiled ||| Full
    where 
      tiled = Tall nmast delta ratio  -- All windows will spaced evenly.
      nmast = 1                       -- Number of windows in master pane.
      ratio = 1 / 2                   -- Size of master pane.
      delta = 3 / 100                 -- % of screen to increment when resizing.  

-- Declare workspaces and rules for applications
myWorkspaces = clickable [ "^i(/home/colin/.xmonad/dzen2/arch_10x10.xbm) term"
		         , "^i(/home/colin/.xmonad/dzen2/fs_01.xbm) web"	
		         , "^i(/home/colin/.xmonad/dzen2/diskette.xbm) docs"
		         , "^i(/home/colin/.xmonad/dzen2/pacman.xbm) games" 
		         , "^i(/home/colin/.xmonad/dzen2/cat.xbm) etc" ]
    where clickable l = [ "^ca(1,xdotool key alt+" ++ show (i) ++ ")" ++ ws ++ "^ca()" |
                          (i,ws) <- zip [1..] l ]
			
myManageHook = composeAll [ resource =? "dmenu"    --> doFloat
			  , resource =? "chromium" --> doShift (myWorkspaces !! 1)
                          , resource =? "gimp"     --> doShift (myWorkspaces !! 2)
                          , resource =? "anki"     --> doShift (myWorkspaces !! 2)
                          , resource =? "evince"   --> doShift (myWorkspaces !! 2)
                          , fmap ("libreoffice" `isInfixOf`) className --> doShift (myWorkspaces !! 2)
                          , fmap ("Steam" `isPrefixOf`) className --> doShift (myWorkspaces !! 3)
                          , resource =? "dolphin-emu" --> doShift (myWorkspaces !! 3)
                          , resource =? "gat" --> doShift (myWorkspaces !! 4) ]

newManageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks 

myLogHook h = dynamicLogWithPP $ defaultPP {
		  ppCurrent         = dzenColor foreground background . pad
		, ppVisible         = dzenColor white0 background . pad
		, ppHidden          = dzenColor white0 background . pad
		, ppHiddenNoWindows = dzenColor black0 background . pad
		, ppWsSep           = ""
		, ppSep             = "   "
		, ppOrder           = \(ws:l:t:_) -> [ws,l]
		, ppOutput          = hPutStrLn h
		, ppLayout          = wrap "^ca(1,xdotool key alt+space)" "^ca()" . dzenColor white1 background .
		                      (\case
				         "Full"                    -> "^i(/home/colin/.xmonad/dzen2/layout_full.xbm)"
				         "Spacing 5 ResizableTall" -> "^i(/home/colin/.xmonad/dzen2/layout_tall.xbm)"
				         "ResizableTall"           -> "^i(/home/colin/.xmonad/dzen2/layout_tall.xbm)"
				         "SimplestFloat"           -> "^i(/home/colin/.xmonad/dzen2/mouse_01.xbm)"
				         "Circle"                  -> "^i(/home/colin/.xmonad/dzen2/full.xbm)"
				         _                         -> "^i(/home/colin/.xmonad/dzen2/grid.xbm)" ) }

myXmonadBar = "dzen2 -x '0' -y '0' -h '12' -w '300' -ta 'l' -fg '" ++ foreground ++ "' -bg '" ++ background ++ "' -fn " ++ myFont

myStatusBar = "conky -qc /home/colin/.xmonad/.conky_dzen | dzen2 -x '300' -w '980' -h '12' -ta 'r' -bg '" ++ background ++ "' -fg '" ++ foreground ++ "' -y '0' -fn " ++ myFont

main = do
  dzenLeftBar  <- spawnPipe myXmonadBar
  dzenRightBar <- spawnPipe myStatusBar
  xmonad $ ewmh defaultConfig
             { terminal           = myTerminal
	     , borderWidth        = 1
	     , normalBorderColor  = black0
             , focusFollowsMouse  = False
	     , focusedBorderColor = magenta0
	     , modMask            = mod4Mask
             , keys               = myKeys
	     , layoutHook         = avoidStruts myLayout
	     , workspaces         = myWorkspaces
	     , manageHook         = newManageHook
	     , handleEventHook    = fullscreenEventHook <+> docksEventHook
	     , startupHook        = setWMName "LG3D"
	     , logHook            = myLogHook dzenLeftBar }

myTerminal 	= "urxvt"
myFont = "xft:lime:size=6"  --":bold:size=8"
--myFont		= "-*-lime-*-*-*-*-*-*-*-*-*-*-*-*"

myKeys conf@(XConfig { XMonad.modMask = modm }) = M.fromList $
    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    -- I CHANGED THIS 2011 OCT 15
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

background= "#000000"
foreground= "#ffffff"

black0= "#343638"
black1= "#404040"

red0=  "#2f468e"
red1=  "#7791e0"

green0= "#424242"
green1= "#828a8c"

yellow0=  "#6b8ba3"
yellow1= "#8ebdde"

blue0=  "#1c4582"
blue1= "#5365a6"

magenta0=  "#74636d"
magenta1= "#927d9e"

cyan0=  "#556c85"
cyan1= "#6e98b8"

white0=  "#b2b2b2"
white1= "#bdbdbd"
