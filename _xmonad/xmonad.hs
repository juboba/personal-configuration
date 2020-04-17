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
import XMonad.Actions.UpdatePointer(updatePointer)

import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

-- import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.ResizableTile
-- import XMonad.Layout.StackTile
-- import XMonad.Layout
-- import XMonad.Layout.Accordion
-- import XMonad.Layout.CenteredMaster
-- import XMonad.Layout.Circle
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
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier(ModifiedLayout)

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
superKey :: KeyMask
superKey = mod4Mask

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "st"

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth   = 1

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
myFocusedBorderColor = "#17729d"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
-- myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
--

goFull :: X ()
goFull = do
    sendMessage $ Toggle FULL
    sendMessage ToggleStruts


myKeys :: [((KeyMask, KeySym), X())]
myKeys =
    -- Toggle Fullscreen
    [ ((superKey, xK_f), goFull)
    , ((superKey .|. shiftMask, xK_f), sendMessage ToggleStruts )
    -- Go to previous workspace
    , ((superKey, xK_Tab), toggleWS)
    -- Cycle tiling modes
    -- , ((superKey, xK_0), sendMessage NextLayout)
    -- Send current workspace to next screen
    , ((superKey .|. shiftMask, xK_o), swapNextScreen)
    -- Focus next screen
    , ((superKey, xK_o), nextScreen)
    -- Focus urgent window
    , ((superKey, xK_x), focusUrgent)

    -- Invert screen colors
    , ((superKey .|. shiftMask, xK_v), spawn "xcalib -i -a")
    -- Screensaver
    , ((superKey .|. shiftMask, xK_z), spawn "slock")
    -- Copy Emoji
    , ((superKey .|. shiftMask, xK_i), spawn $ "rofiemoji")
    -- Launch rofi
    , ((superKey, xK_p), spawn "rofi -show drun -modi drun,ssh -disable-history -sort -levenshtein-sort")
    -- Toggle window transparency
    , ((superKey, xK_r), spawn "transset 0.8 -t")
    -- Launch Terminal
    -- , ((modm,               xK_F1     ), spawn $ XMonad.terminal conf)
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
    [ isFullscreen --> doFullFloat
    , className =? "Emacs"                     --> (doShift $ myWorkspaces !! 1)
    , className =? "Thunderbird"               --> (doShift $ myWorkspaces !! 7)
    , className =? "Chromium"                  --> (doShift $ myWorkspaces !! 2)
    , title =? "Spotify Premium"               --> (doShift $ myWorkspaces !! 5)
    , className =? "TelegramDesktop"           --> (doShift $ myWorkspaces !! 6)
    , className =? "Microsoft Teams - Preview" --> (doShift $ myWorkspaces !! 6)
    , className =? "Firefox"                   --> (doShift $ myWorkspaces !! 8)
    , className =? "XClock"                    --> doIgnore
    , stringProperty "_NET_WM_STATE(ATOM)" =? "_NET_WM_STATE_SKIP_TASKBAR"                    --> doIgnore
    -- , resource  =? "desktop_window"            --> doIgnore
    -- , className =? "Exe"                       --> doFloat
    -- , className =? "Gvim"                      --> viewShift "^ vim"
    -- , className =? "Gimp"                      --> doShift "8 grphx" <+> doFloat
    , title =? "Picture-in-Picture"            --> doFloat -- Firefox videos
    , title =? "Media viewer"                  --> doFloat -- Telegram media
    , title =? "Microsoft Teams Notification"  --> doFloat -- Teams notifications
    , className =? "Gsimplecal"                --> doFloat -- Calendar window
    , title     =? "Copying Files"             --> doFloat ]
    -- where viewShift = doF . liftM2 (.) W.greedyView W.shift

------------------------------------------------------------------------
-- Layouts:
mainLayout :: ModifiedLayout SmartSpacing Tall a
mainLayout = smartSpacing 10 $ Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2

-- tallLayout :: ModifiedLayout
tallLayout       = named "\61659"     $ avoidStruts $ mainLayout -- 
wideLayout       = named "\61785"     $ avoidStruts $ Mirror mainLayout -- 
-- singleLayout     = named "\61640"   $ avoidStruts $ noBorders Full -- 
-- circleLayout     = named "\61713"   $ Circle -- 
-- twoPaneLayout    = named "Two Pane" $ TwoPane (2/100) (1/2)
-- mosaicLayout     = named "Mosaic"   $ MosaicAlt M.empty
-- gridLayout       = named "Grid"     $ Grid
-- spiralLayout     = named "Spiral"   $ spiral (1 % 1)

-- myLayoutHook :: ModifiedLayout ??
myLayoutHook = tallLayout
  ||| wideLayout
  -- ||| singleLayout
  -- ||| twoPaneLayout
  -- ||| circleLayout
  -- ||| mosaicLayout
  -- ||| gridLayout
  -- ||| spiralLayout

currentWsStyle :: String -> String
currentWsStyle = xmobarColor "#ffc145" ""

layoutIndicatorStyle :: String -> String
layoutIndicatorStyle = wrap "" "" . xmobarColor "#006494" ""

urgentWsIndicatorStyle :: String -> String
urgentWsIndicatorStyle = xmobarColor "#red4" "" . xmobarStrip

visibleWsStyle :: String -> String
visibleWsStyle = xmobarColor "#ba8d33" "LightSkyBlue4"

windowTitleStyle :: String -> String
windowTitleStyle = xmobarColor "#de3c4b" "" . shorten 20

main :: IO()
main = do
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
                        , ppSep     = "  "
                        , ppTitle   = windowTitleStyle
                        , ppUrgent  = urgentWsIndicatorStyle
                        , ppVisible = visibleWsStyle
                        , ppWsSep   = "  "
                        }
                        >> updatePointer (0.5, 0.5) (0, 0)
        , manageHook        = manageDocks <+> myManageHook <+> manageHook def
        , modMask           = superKey
        , normalBorderColor = myNormalBorderColor
        , terminal          = myTerminal
        , workspaces        = myWorkspaces
        } `additionalKeys` myKeys