import System.IO

-- import Data.Ratio
-- import Control.Monad (liftM2)

import XMonad
-- import XMonad.Config.Gnome
import Data.Monoid
-- import System.Exit

import XMonad.Actions.CycleWS
-- import XMonad.Actions.CycleRecentWS
-- import XMonad.Actions.FloatKeys
import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.EwmhDesktops
-- import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

-- import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.ResizableTile
-- import XMonad.Layout.StackTile
-- import XMonad.Layout
-- import XMonad.Layout.Accordion
-- import XMonad.Layout.CenteredMaster
import XMonad.Layout.Circle
-- import XMonad.Layout.Grid
-- import XMonad.Layout.IM
-- import XMonad.Layout.MosaicAlt
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
-- import XMonad.Layout.Reflect
-- import XMonad.Layout.Spiral
-- import XMonad.Layout.Tabbed
-- import XMonad.Layout.TwoPane

-- import XMonad.Config.Desktop

-- import qualified XMonad.StackSet as W

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

-- import qualified Data.Map        as M

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
-- myModMask       = mod4Mask
myModMask :: KeyMask
myModMask = mod4Mask

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "gnome-terminal"

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth   = 5

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces    = [ "\61728" -- 
                  , "\61508" -- 
                  , "\62056" -- 
                  , "\61888" -- 
                  , "\61760" -- 
                  , "\61477" -- ''
                  , "\61574" -- 
                  , "\61664" -- 
                  , "\62057" -- 
                  ]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor :: String
myNormalBorderColor  = "#000000"

myFocusedBorderColor :: String
myFocusedBorderColor = "#93c247"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
-- myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
--
scrotCmd :: String
scrotCmd = "sleep 2; scrot 'At_%Y_%m_%d_%H-%M-%S.png' -e 'mv $f ~/Pictures/Screenshots'"

myKeys :: [((KeyMask, KeySym), X())]
myKeys =
    -- Screensaver
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    -- Toggle Fullscreen
    , ((mod4Mask, xK_f), sendMessage $ Toggle FULL)
    -- Go to previous workspace
    , ((mod4Mask, xK_Tab), toggleWS)
    -- Windowshot
    , ((mod4Mask .|. shiftMask, xK_s), spawn $ scrotCmd ++ " -s")
    -- Screenshot
    , ((mod4Mask, xK_s), spawn scrotCmd)
    -- Cycle tiling modes
    , ((mod4Mask, xK_0), sendMessage NextLayout)
    -- Reverse cycle tiling modes
    , ((mod4Mask .|. shiftMask, xK_0), sendMessage FirstLayout)
    -- Launch rofi
    , ((mod4Mask, xK_space), spawn "rofi -show run -disable-history -sort -levenshtein-sort")
    -- , ((modm,               xK_F1     ), spawn $ XMonad.terminal conf)
    -- Focus urgent window
    , ((mod4Mask             , xK_x      ), focusUrgent)
    ]

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "Emacs"                     --> (doShift $ myWorkspaces !! 1)
    , className =? "Thunderbird"               --> (doShift $ myWorkspaces !! 7)
    , className =? "Chromium"                  --> (doShift $ myWorkspaces !! 2)
    , title     =? "Spotify"                   --> (doShift $ myWorkspaces !! 5)
    , className =? "TelegramDesktop"           --> (doShift $ myWorkspaces !! 6)
    , className =? "Microsoft Teams - Preview" --> (doShift $ myWorkspaces !! 6)
    , className =? "Firefox"                   --> (doShift $ myWorkspaces !! 8)
    , className =? "XClock"                    --> doIgnore
    , stringProperty "_NET_WM_STATE(ATOM)" =? "_NET_WM_STATE_SKIP_TASKBAR"                    --> doIgnore
    -- , resource  =? "desktop_window"            --> doIgnore
    -- , className =? "Exe"                       --> doFloat
    -- , className =? "Gvim"                      --> viewShift "^ vim"
    -- , className =? "Gimp"                      --> doShift "8 grphx" <+> doFloat

    , title     =? "Copying Files"             --> doFloat ]
    -- where viewShift = doF . liftM2 (.) W.greedyView W.shift

------------------------------------------------------------------------
-- Layouts:
basicLayout :: Tall a
basicLayout = Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2
-- tallLayout :: ModifiedLayout
tallLayout       = named "\61659"     $ avoidStruts $ basicLayout -- 
wideLayout       = named "\61785"     $ avoidStruts $ Mirror basicLayout -- 
singleLayout     = named "\61640"   $ avoidStruts $ noBorders Full -- 
circleLayout     = named "\61713"   $ Circle -- 
-- twoPaneLayout    = named "Two Pane" $ TwoPane (2/100) (1/2)
-- mosaicLayout     = named "Mosaic"   $ MosaicAlt M.empty
-- gridLayout       = named "Grid"     $ Grid
-- spiralLayout     = named "Spiral"   $ spiral (1 % 1)

myLayoutHook = tallLayout
  ||| wideLayout
  ||| singleLayout
  -- ||| twoPaneLayout
  ||| circleLayout
  -- ||| mosaicLayout
  -- ||| gridLayout
  -- ||| spiralLayout

currentWsStyle :: String -> String
currentWsStyle = xmobarColor "DeepSkyBlue" ""

layoutIndicatorStyle :: String -> String
layoutIndicatorStyle = wrap "  " "  " . xmobarColor "DarkOrange" ""

urgentWsIndicatorStyle :: String -> String
urgentWsIndicatorStyle = xmobarColor "#red4" "" . xmobarStrip

visibleWsStyle :: String -> String
visibleWsStyle = xmobarColor "Red" "LightSkyBlue4"

windowTitleStyle :: String -> String
windowTitleStyle = xmobarColor "DeepPink" "" . shorten 40

main :: IO()
main = do
    _ <- spawnPipe "stalonetray"
    _ <- spawnPipe "pasystray"
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $ withUrgencyHook NoUrgencyHook def
        { borderWidth        = myBorderWidth
        , focusedBorderColor = myFocusedBorderColor
        , layoutHook         = smartBorders . avoidStruts
          $ mkToggle (NOBORDERS ?? FULL ?? EOT)
          $ myLayoutHook
        , logHook            = dynamicLogWithPP xmobarPP
                        { ppCurrent = currentWsStyle
                        , ppLayout  = layoutIndicatorStyle
                        , ppOutput  = hPutStrLn xmproc
                        , ppSep     = ""
                        , ppTitle   = windowTitleStyle
                        , ppUrgent  = urgentWsIndicatorStyle
                        , ppVisible = visibleWsStyle
                        , ppWsSep   = "  "
                        }
        , manageHook        = manageDocks <+> myManageHook <+> manageHook def
        , modMask           = myModMask
        , normalBorderColor = myNormalBorderColor
        , terminal          = myTerminal
        , workspaces        = myWorkspaces
        } `additionalKeys` myKeys
