{-# LANGUAGE LambdaCase #-}

import           Data.List
import qualified Data.Map as M
import           Data.Monoid
import           GHC.IO.Handle.Types
import           System.Exit (exitSuccess)
import           System.IO (hPutStrLn)
import           XMonad
import           XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import           XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import           XMonad.Util.Run (spawnPipe)

---

-- Consider `smartSpacing 5`
myLayout :: Choose Tall (Choose (Mirror Tall) Full) a
myLayout = tiled ||| Mirror tiled ||| Full
  where tiled = Tall nmast delta ratio  -- All windows will spaced evenly
        nmast = 1                       -- Number of windows in master pane
        delta = 3 / 100                 -- % of screen to increment on resize
        ratio = 1 / 2                   -- Size of master pane

myWorkspaces :: [String]
myWorkspaces = [code, web, devel, media, misc]
  where code  = "   ^i(/home/colin/.xmonad/icons/code.xbm)   "
        web   = "   ^i(/home/colin/.xmonad/icons/www.xbm)   "
        devel = "   ^i(/home/colin/.xmonad/icons/Devel.xbm)   "
        media = "   ^i(/home/colin/.xmonad/icons/media.xbm)   "
        misc  = "   ^i(/home/colin/.xmonad/icons/pacman.xbm)   "

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [ resource =? "dmenu"    --> doFloat
  , className =? "chromium" --> doShift (myWorkspaces !! 1)
  , className =? "firefox"  --> doShift (myWorkspaces !! 1)
  , resource =? "krita"     --> doShift (myWorkspaces !! 2)
  , resource =? "evince"   --> doShift (myWorkspaces !! 2)
  , fmap ("libreoffice" `isInfixOf`) className --> doShift (myWorkspaces !! 2)
  , fmap ("Steam" `isPrefixOf`) className --> doShift (myWorkspaces !! 3)
  , resource =? "dolphin-emu" --> doShift (myWorkspaces !! 3)
  , fmap ("Battle" `isInfixOf`) className --> doShift (myWorkspaces !! 3)
  , resource =? "qutebrowser" --> doShift (myWorkspaces !! 1)
  ]

newManageHook :: Query (Endo WindowSet)
newManageHook = manageHook def <+> manageDocks <+> myManageHook

myLogHook :: Handle -> X ()
myLogHook dlb = dynamicLogWithPP $ dzenPP {
  ppOutput = hPutStrLn dlb
  , ppTitle =  wrap "^ca(1,/home/colin/.xmonad/scripts/popterm.sh)" "^ca()" . pad  . shorten 50
  , ppLayout = dzenColor color4 background
    . (\x -> case x of
          "Tall" -> "    ^i(/home/colin/.xmonad/icons/tiling.xbm)   "
          "Mirror Tall" -> "    ^i(/home/colin/.xmonad/icons/mirrortall.xbm)   "
          "Full" -> "    ^i(/home/colin/.xmonad/icons/floating.xbm)   "
          _ -> "   New Layout   "
      )

  , ppCurrent = dzenColor foreground background -- foreground "#FF6000"
  , ppVisible = dzenColor color4 background
  , ppHidden = dzenColor color4 background  -- foreground "#7BB352"
  , ppHiddenNoWindows = dzenColor color8 background
  , ppOrder = \(ws:l:t:_) -> [ws,l,t]
  }

myStatusBar :: String
myStatusBar = "dzen2 -x 0 -w '683' -h '28' -ta l -xs 1 -fg '" ++ foreground ++ "' -bg '" ++ background ++ "' -fn '" ++ myFont ++ "'"

myConkyBar :: String
myConkyBar = "conky -c ~/.xmonad/conky_dzen | dzen2 -ta r -x '683' -w '683' -h '28' -p $OPTS -xs 1 -fg '" ++ foreground ++ "' -bg '" ++ background ++ "' -fn '" ++ myFont ++ "'"

myFont :: String
myFont = "-*-ubuntu-*-*-*-*-12-*-*-*-*-*-*-*"

main :: IO ()
main = do
  dzenLeftBar  <- spawnPipe myStatusBar
  dzenRightBar <- spawnPipe myConkyBar
  xmonad $ myConfig dzenLeftBar
--  xmonad . ewmh $ myConfig dzenLeftBar

myConfig dlb = def
  { terminal           = myTerminal
  , borderWidth        = 1
  , normalBorderColor  = black0
  , focusFollowsMouse  = False
  , focusedBorderColor = red1
  , modMask            = mod4Mask
  , keys               = myKeys
  , layoutHook         = smartBorders $ avoidStruts myLayout
  , workspaces         = myWorkspaces
  , manageHook         = newManageHook
  , handleEventHook    = fadeWindowsEventHook <+> fullscreenEventHook <+> docksEventHook
  , startupHook        = setWMName "LG3D"
  , logHook            = myLogHook dlb >> setWMName "LG3D" }
  
myTerminal :: String
myTerminal = "urxvt"

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
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
  , ((modm,               xK_k     ), refresh)

    -- Move focus to the next window
  , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
  , ((modm,               xK_n     ), windows W.focusDown)

    -- Move focus to the previous window
  , ((modm,               xK_e     ), windows W.focusUp  )

    -- Move focus to the master window
  , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
  , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_n     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_e     ), windows W.swapUp    )

    -- Shrink the master area
  , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
  , ((modm,               xK_o     ), sendMessage Expand)

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
  | (key, sc) <- zip [xK_w, xK_r] [0..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

black0 = "#343638"
black1 = "#404040"

red0 =  "#2f468e"
red1 =  "#7791e0"

green0 = "#424242"
green1 = "#828a8c"

yellow0 =  "#6b8ba3"
yellow1 = "#8ebdde"

blue0 =  "#1c4582"
blue1 = "#5365a6"

magenta0 =  "#74636d"
magenta1 = "#927d9e"

cyan0 =  "#556c85"
cyan1 = "#6e98b8"

white0 =  "#b2b2b2"
white1 = "#bdbdbd"

background= "#232323" -- "#0E0E0E"
foreground= "#CBCBCB"
color0= "#454545"
color8= "#676767"
color1=  "#CC4747"
color9=  "#BF5858"
color2=  "#A0CF5D"
color10= "#B8D68C"
color3=  "#FF9B52"
color11= "#FFB680"
color4=  "#307D92" -- "#508934" -- "#AB2010" -- "#99492F"
color12= "#99C7BF"
color5=  "#A966CC"
color13= "#BD9FCC"
color6=  "#6CAB79"
color14= "#95BA9C"
color7=  "#d3d3d3"
color15= "#fefefe"
