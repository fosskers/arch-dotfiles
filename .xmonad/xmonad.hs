import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

import Data.Monoid
import System.Exit
import System.Random (getStdGen,randomR,StdGen(..))

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


main = do
  gen <- getStdGen
  let borderColour = randomColour gen
  xmonad $ defaultConfig
       { terminal           = myTerminal
       , modMask            = mod4Mask
       , borderWidth        = myBorderWidth
       , focusFollowsMouse  = myFocusFollowsMouse
       , focusedBorderColor = borderColour
       , keys               = myKeys
       , layoutHook         = myLayout
       , workspaces         = myWorkspaces
       , manageHook         = myManageHook <+> manageHook defaultConfig
       }

----------------
-- RANDOM COLOUR
----------------
randomColour :: StdGen -> String
randomColour gen = '#' : take 6 (yieldRandoms gen hexValues)

hexValues :: String
hexValues = "0123456789abcdef"

-- Infinitely yields a random value from a given list.
yieldRandoms :: StdGen -> [a] -> [a]
yieldRandoms _ []   = []
yieldRandoms gen xs = (xs !! pos) : yieldRandoms gen' xs
    where (pos,gen') = randomR (0, length xs - 1) gen

-------------------
-- TRIVIAL SETTINGS
-------------------
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myFocusedBorderColor :: String
myFocusedBorderColor = "#456def"  -- "#0099cc"

myTerminal :: String
myTerminal = "urxvt"

myBorderWidth = 1

------------------
-- LAYOUT SETTINGS
------------------
defaultLayouts = tiled ||| Mirror tiled ||| Full
    where 
      tiled = Tall nmast delta ratio  -- All windows will spaced evenly.
      nmast = 1                       -- Number of windows in master pane.
      ratio = 1 / 2                   -- Size of master pane.
      delta = 3 / 100                 -- % of screen to increment when resizing.  

nobordersLayout = noBorders $ Full

myLayout = onWorkspace "9:Skyrim" nobordersLayout $ defaultLayouts

---------------------
-- WORKSPACE SETTINGS
---------------------
myWorkspaces :: [String]
myWorkspaces = ["1","2:web","3","4","5","6","7","8","9:Skyrim"]

-- This isn't working.
myManageHook = composeAll
               [ className =? "chromium" --> doShift "2:web"
               , className =? "Skyrim" --> doShift "9:Skyrim"
               ]

---------------
-- KEY BINDINGS
---------------
-- TAKEN FROM SAMPLE SOURCE IN ~/usr/share/xmonad-0.9.2/man/xmonad.hs
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

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
    , ((modm,               xK_Return), windows W.swapMaster)

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
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

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

