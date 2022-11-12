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
import XMonad.Hooks.FadeInactive

-- import XMonad.Layout.Renamed
import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.ResizableTile
-- import XMonad.Layout.StackTile
-- import XMonad.Layout
import XMonad.Layout.Accordion
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

import qualified XMonad.StackSet as W

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
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
myTerminal = "alacritty"

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
myBorderWidth = 0

-- scratchPads
scratchpads :: [NamedScratchpad]
scratchpads = [ NS "terminal" "alacritty --class scratch-term -e jmux" (appName =? "scratch-term") (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4)) ]


xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

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
myWorkspaces = clickable . map xmobarEscape $ [ " \61728 " -- 
                  , " \61508 " -- 
                  , " \62038 " -- 
                  , " \62056 " -- 
                  , " \61574 " -- 
                  , " \61477 " -- ''
                  , " \61664 " -- 
                  , " \61760 " -- 
                  , " \62057 " -- 
                  ]
               where clickable w = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>"
                                     | (i,ws) <- zip [1..9] w, let n = i ]

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
    , ((superKey .|. shiftMask, xK_f), sendMessage ToggleStruts )
    , ((superKey, xK_Left), moveTo Prev (Not emptyWS))
    , ((superKey, xK_Right), moveTo Next (Not emptyWS))
    -- Go to previous workspace
    , ((superKey, xK_Tab), toggleWS' ["NSP"])
    --, ((superKey, xK_g), goToSelected defaultConfig)
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

    -- Suspend
    , ((superKey .|. shiftMask, xK_x), spawn "systemctl suspend")
    -- Open Pavucontrol
    , ((superKey, xK_v), spawn "pavucontrol -t 3")
    -- Invert screen colors
    , ((superKey .|. shiftMask, xK_v), spawn "invert_colors")
    -- Screensaver
    , ((superKey .|. shiftMask, xK_z), spawn "slock")
    -- Copy Emoji
    , ((superKey .|. shiftMask, xK_i), spawn "rofiemoji")
    -- Clipboard Menu
    , ((superKey .|. shiftMask, xK_m), spawn "clipmenu")
    -- Launch color picker
    , ((superKey .|. shiftMask, xK_y), spawn "pick-colour-picker")
    -- Set slack status
    , ((superKey .|. shiftMask, xK_l), spawn "slack-do")
    -- Select screen
    , ((superKey, xK_p), spawn "smod")
    -- Toggle terminal scratchpad
    , ((superKey .|. shiftMask, xK_t), namedScratchpadAction scratchpads "terminal")
    -- Launch Screenshot
    , ((0, xK_Print), spawn "flameshot gui")
    -- Launch Volatile Screenshot
    , ((shiftMask, xK_Print), spawn "sleep 1; sshot -t")
    -- Media keys
    , ((0, xF86XK_AudioMute), spawn "vol mute")
    , ((0, xF86XK_AudioRaiseVolume), spawn "vol up")
    , ((0, xF86XK_AudioLowerVolume), spawn "vol down")
    , ((0, xF86XK_AudioPlay), spawn "sp play")
    , ((0, xF86XK_AudioPrev), spawn "sp prev")
    , ((0, xF86XK_AudioNext), spawn "sp next")
    , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl s 10%+")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10%-")
    -- , ((shiftMask, xF86XK_KbdBrightnessUp), spawn "shine temp-up")
    -- , ((shiftMask, xF86XK_KbdBrightnessDown), spawn "shine temp-down")
    -- Launch rofi
    , ((superKey, xK_e), spawn myLauncher)
    -- Toggle window transparency
    , ((superKey, xK_r), spawn "transset 0.8 -t -a")
    -- Notifications
    , ((superKey .|. shiftMask, xK_n), spawn "dunstctl set-paused toggle")
    , ((superKey, xK_grave), spawn "dunstctl close-all")
    , ((superKey .|. shiftMask, xK_grave), spawn "dunstctl history-pop")
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
    , className =? "Emacs"                                                 --> takeTo 1
    , className =? "Thunderbird"                                           --> takeTo 7
    -- , className =? "Xmessage"                                              --> doFloat
    , appName =? "chromium-browser (dev-profile)"                          --> takeTo 2
    , appName =? "chromium-browser"                                        --> takeTo 3
    , appName =? "google-chrome"                                           --> takeTo 3
    , title =? "meet.google.com is sharing your screen."                   --> takeTo 6
    , className =? "Spotify"                                               --> takeTo 5
    , className =? "TelegramDesktop"                                       --> takeTo 4
    , className =? "Slack"                                                 --> takeTo 4
    , className =? "discord"                                               --> takeTo 4
    , className =? "firefox"                                               --> takeTo 8
    , stringProperty "_NET_WM_STATE(ATOM)" =? "_NET_WM_STATE_SKIP_TASKBAR" --> doIgnore
    -- , resource  =? "desktop_window"                                     --> doIgnore
    -- , className =? "Exe"                                                --> doFloat
    -- , className =? "Gvim"                                               --> viewShift "^ vim"
    -- , className =? "Gimp"                                               --> doShift "8 grphx" <+> doFloat
    , title =? "Picture-in-Picture"                                        --> doFloat -- Firefox videos
    , className =? "Screen"                                                --> doFloat -- Screen share (with Screen.so)
    , title =? "Media viewer"                                              --> doFloat -- Telegram media
    , className =? "Gsimplecal"                                            --> doFloat -- Calendar window
    , appName =? "plasmashell"                                             --> doIgnore -- Plasma stuff
    , className =? ".pick-colour-picker-wrapped"                           --> doFloat -- Color picker
    , className =? "Pavucontrol"                                           --> doFloat
    , appName =? "showmyself"                                              --> doFloat -- Show me
    , title     =? "Copying Files"                                         --> doFloat
    , className =? "Xmessage"                                              --> doFloat
    , checkDock                                                            --> doLower]
    where takeTo n = doShift $ myWorkspaces !! n
    -- where viewShift = doF . liftM2 (.) W.greedyView W.shift

------------------------------------------------------------------------
-- Startup:
myStartupHook :: X ()
myStartupHook =
  spawnOnce "fusuma" <+>
  spawnOnce "trayer --height 30 --width 6 --edge top --align right --tint 0x00000000 --transparent true --alpha 0"
-- do
--   _ <- let count = show  wsContainingCopies
--    in spawn ("notify-send " ++ count)
--   return ()

------------------------------------------------------------------------
addGap = smartSpacing 10

-- Layouts:
mainLayout :: ModifiedLayout Spacing Tall a
mainLayout = addGap $ Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 3/4

-- tallLayout :: ModifiedLayout
tallLayout       = named "\61659"     $ avoidStruts mainLayout -- 
wideLayout       = named "\61785"     $ avoidStruts $ Mirror mainLayout -- 

messagingLayout  = named "\61659" $ addGap $ Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 4/5

-- singleLayout     = named "\61640"   $ avoidStruts $ noBorders Full -- 
-- circleLayout     = named "\61713"   $ Circle -- 
-- twoPaneLayout    = named "Two Pane" $ TwoPane (2/100) (1/2)
-- mosaicLayout     = named "Mosaic"   $ MosaicAlt M.empty
-- gridLayout       = named "Grid"     $ Grid
-- spiralLayout     = named "Spiral"   $ spiral (6/7) -- (1 % 1)
accordionLayout  = named "A" $ avoidStruts $ addGap Accordion
verticalAccordionLayout  = named "VA" $ avoidStruts $ addGap $Mirror Accordion

-- myLayoutHook :: ModifiedLayout ??
myLayoutHook = onWorkspace (myWorkspaces !! 4) messagingLayout
  tallLayout
  ||| wideLayout
  ||| accordionLayout
  ||| verticalAccordionLayout
  -- ||| singleLayout
  -- ||| twoPaneLayout
  -- ||| circleLayout
  -- ||| mosaicLayout
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
windowTitleStyle = xmobarColor "#444444" "" . shorten 30 . xmobarStrip . wrap " " " "

main :: IO()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks $ withUrgencyHook NoUrgencyHook def
        { borderWidth        = myBorderWidth
        , focusedBorderColor = myFocusedBorderColor
        , layoutHook         = smartBorders . avoidStruts
          $ mkToggle (NOBORDERS ?? FULL ?? EOT)
          myLayoutHook
        , logHook = fadeInactiveLogHook 0.8 <+> dynamicLogWithPP xmobarPP
                        { ppCurrent = currentWsStyle
                        , ppLayout  = layoutIndicatorStyle
                        , ppOutput  = hPutStrLn xmproc
                        , ppSep     = "  "
                        , ppSort = (.namedScratchpadFilterOutWorkspace) <$> ppSort xmobarPP
                        , ppTitle   = windowTitleStyle
                        , ppUrgent  = urgentWsIndicatorStyle
                        , ppVisible = visibleWsStyle
                        , ppWsSep   = " "
                        }
                        >> updatePointer (0.5, 0.5) (0, 0)
        , manageHook        = manageDocks
                              <+> insertPosition Master Newer
                              <+> myManageHook
                              <+> namedScratchpadManageHook scratchpads
                              <+> manageHook def
        , modMask           = superKey
        , normalBorderColor = myNormalBorderColor
        , startupHook       = myStartupHook
        , terminal          = myTerminal
        , workspaces        = myWorkspaces
        } `additionalKeys` myKeys
