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
-- import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
-- import XMonad.Actions.SpawnOn

import XMonad.Actions.CopyWindow

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

-- import XMonad.Layout.Renamed
import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.ResizableTile
-- import XMonad.Layout.StackTile
-- import XMonad.Layout
-- import XMonad.Layout.Accordion
-- import XMonad.Layout.CenteredMaster
-- import XMonad.Layout.Circle
-- import XMonad.Layout.Grid
-- import XMonad.Layout.IM
import XMonad.Layout.MosaicAlt
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
import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map        as M

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
myTerminal = "terminal"

-- My application launcher
--
myLauncher :: String
myLauncher = "rofi -show-icons -show drun -modi drun,ssh -disable-history -sort -levenshtein-sort -display-drun Run"

-- showMe will spawn a little window with my face in it =)
-- showMe :: X ()
-- the ManageHook here is not working. Don't know why...
-- showMe = spawnAndDo doIgnore "mplayer -vf mirror tv:// -tv device=/dev/video2:width=320:height=240"

-- launchVim will spawn a vim window with the selectd file
launchVim :: X ()
launchVim = spawn "rofivim"

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth   = 2

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
myWorkspaces    = [ " \61728 " -- 
                  , " \61508 " -- 
                  , " \62038 " -- 
                  , " \62056 " -- 
                  , " \61574 " -- 
                  , " \61477 " -- ''
                  , " \61760 " -- 
                  , " \61664 " -- 
                  , " \62057 " -- 
                  ]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor :: String
myNormalBorderColor  = "#000000"

myFocusedBorderColor :: String
myFocusedBorderColor = "#30bced"

goFull :: X ()
goFull = do
    sendMessage $ Toggle FULL
    sendMessage ToggleStruts

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
-- myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
--

myKeys :: [((KeyMask, KeySym), X())]
myKeys =
    -- Toggle Fullscreen
    [ ((superKey, xK_f), goFull)
    , ((superKey, xK_Left), moveTo Prev NonEmptyWS)
    , ((superKey, xK_Right), moveTo Next NonEmptyWS)
    , ((superKey .|. shiftMask, xK_f), sendMessage ToggleStruts )
    -- Go to previous workspace
    , ((superKey, xK_Tab), toggleWS)
    , ((superKey, xK_g), goToSelected defaultGSConfig)
    -- Cycle tiling modes
    -- , ((superKey, xK_0), sendMessage NextLayout)
    -- Send current workspace to next screen
    , ((superKey .|. shiftMask, xK_o), swapNextScreen)
    , ((superKey .|. shiftMask, xK_e), launchVim)
    , ((superKey .|. shiftMask, xK_w), spawn "rofisxiv")
    -- Focus next screen
    , ((superKey, xK_o), nextScreen)
    , ((superKey, xK_s ), windows copyToAll) -- @@ Make focused window always visible
    , ((superKey .|. shiftMask, xK_s ),  killAllOtherCopies) -- @@ Toggle window state back
    -- Focus urgent window
    , ((superKey, xK_x), focusUrgent)

    -- Invert screen colors
    , ((superKey .|. shiftMask, xK_v), spawn "xcalib -i -a")
    -- Screensaver
    , ((superKey .|. shiftMask, xK_z), spawn "slock")
    -- Copy Emoji
    , ((superKey .|. shiftMask, xK_i), spawn "rofiemoji")
    -- Clipboard Menu
    , ((superKey .|. shiftMask, xK_m), spawn "clipmenu")
    -- Launch color picker
    , ((superKey .|. shiftMask, xK_y), spawn "pick-colour-picker")
    -- Launch Screenshot
    , ((0, xK_Print), spawn "sleep 1; sshot")
    -- Launch Volatile Screenshot
    , ((shiftMask, xK_Print), spawn "sleep 1; sshot -t")
    -- Media keys
    , ((0, xF86XK_AudioMute), spawn "vol mute")
    , ((0, xF86XK_AudioRaiseVolume), spawn "vol up")
    , ((0, xF86XK_AudioLowerVolume), spawn "vol down")
    , ((0, xF86XK_AudioPlay), spawn "sp play")
    , ((0, xF86XK_AudioPrev), spawn "sp prev")
    , ((0, xF86XK_AudioNext), spawn "sp next")
    , ((0, xF86XK_KbdBrightnessUp), spawn "shine brightness-up")
    , ((0, xF86XK_KbdBrightnessDown), spawn "shine brightness-down")
    , ((shiftMask, xF86XK_KbdBrightnessUp), spawn "shine temp-up")
    , ((shiftMask, xF86XK_KbdBrightnessDown), spawn "shine temp-down")
    -- Launch rofi
    , ((superKey, xK_p), spawn myLauncher)
    -- Toggle window transparency
    , ((superKey, xK_r), spawn "transset 0.8 -t -a")
    -- Toggle notifications
    , ((superKey .|. shiftMask, xK_n), spawn "notify-send \"DUNST_COMMAND_TOGGLE\"")
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
    , className =? "Emacs"                                                 --> (doShift $ myWorkspaces !! 1)
    , className =? "Thunderbird"                                           --> (doShift $ myWorkspaces !! 7)
    -- , className =? "Xmessage"                                              --> doFloat
    , appName =? "chromium-browser (dev-profile)"                          --> (doShift $ myWorkspaces !! 2)
    , appName =? "chromium-browser"                                        --> (doShift $ myWorkspaces !! 3)
    , title =? "meet.google.com is sharing your screen."                   --> (doShift $ myWorkspaces !! 7)
    , className =? "Spotify"                                               --> (doShift $ myWorkspaces !! 5)
    , className =? "TelegramDesktop"                                       --> (doShift $ myWorkspaces !! 4)
    , className =? "Slack"                                                 --> (doShift $ myWorkspaces !! 4)
    , className =? "Firefox"                                               --> (doShift $ myWorkspaces !! 8)
    , stringProperty "_NET_WM_STATE(ATOM)" =? "_NET_WM_STATE_SKIP_TASKBAR" --> doIgnore
    -- , resource  =? "desktop_window"                                     --> doIgnore
    -- , className =? "Exe"                                                --> doFloat
    -- , className =? "Gvim"                                               --> viewShift "^ vim"
    -- , className =? "Gimp"                                               --> doShift "8 grphx" <+> doFloat
    , title =? "Picture-in-Picture"                                        --> doFloat -- Firefox videos
    , className =? "Screen"                                                --> doFloat -- Screen share (with Screen.so)
    , title =? "Media viewer"                                              --> doFloat -- Telegram media
    , className =? "Gsimplecal"                                            --> doFloat -- Calendar window
    , className =? ".pick-colour-picker-wrapped"                           --> doFloat -- Calendar window
    , title     =? "Copying Files"                                         --> doFloat ]
    -- where viewShift = doF . liftM2 (.) W.greedyView W.shift

------------------------------------------------------------------------
-- Startup:
myStartupHook :: X ()
myStartupHook = do
  -- _ <- let count = show  wsContainingCopies
  --  in spawn ("notify-send " ++ count)
  return ()

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

messagingLayout  = named "\61659" $ smartSpacing 10 $ Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 3/4

-- singleLayout     = named "\61640"   $ avoidStruts $ noBorders Full -- 
-- circleLayout     = named "\61713"   $ Circle -- 
-- twoPaneLayout    = named "Two Pane" $ TwoPane (2/100) (1/2)
mosaicLayout     = named "Mosaic"   $ MosaicAlt M.empty
-- gridLayout       = named "Grid"     $ Grid
-- spiralLayout     = named "Spiral"   $ spiral (6/7) -- (1 % 1)

-- myLayoutHook :: ModifiedLayout ??
myLayoutHook = onWorkspace (myWorkspaces !! 4) messagingLayout
  tallLayout
  ||| wideLayout
  -- ||| singleLayout
  -- ||| twoPaneLayout
  -- ||| circleLayout
  ||| mosaicLayout
  -- ||| gridLayout
  -- ||| spiralLayout

currentWsStyle :: String -> String
currentWsStyle = xmobarColor "#30bced" ""

urgentWsIndicatorStyle :: String -> String
urgentWsIndicatorStyle = xmobarColor "#fc5130" ""

visibleWsStyle :: String -> String
visibleWsStyle = xmobarColor "#20aaaa" "LightSkyBlue4"

layoutIndicatorStyle :: String -> String
layoutIndicatorStyle = wrap "" "" . xmobarColor "#303036" ""

windowTitleStyle :: String -> String
windowTitleStyle = xmobarColor "#fffaff" "" . shorten 30 . xmobarStrip . wrap " " " "

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
                        , ppWsSep   = " "
                        }
                        >> updatePointer (0.5, 0.5) (0, 0)
        , manageHook        = manageDocks
                              <+> insertPosition Master Newer
                              <+> myManageHook
                              <+> manageHook def
        , modMask           = superKey
        , normalBorderColor = myNormalBorderColor
        , startupHook       = myStartupHook
        , terminal          = myTerminal
        , workspaces        = myWorkspaces
        } `additionalKeys` myKeys
