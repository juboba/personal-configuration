-- import Data.Ratio
-- import Control.Monad (liftM2)

-- import XMonad.Config.Gnome

-- import System.Exit

-- import XMonad.Actions.CycleRecentWS
-- import XMonad.Actions.DynamicWorkspaces
-- import XMonad.Actions.FloatKeys
-- import XMonad.Actions.GridSelect
-- import XMonad.Actions.SpawnOn

-- import XMonad.Hooks.SetWMName

-- import XMonad.Layout
-- import XMonad.Layout.CenteredMaster
-- import XMonad.Layout.Circle
-- import XMonad.Layout.Grid
-- import XMonad.Layout.IM
-- import XMonad.Layout.Reflect
-- import XMonad.Layout.Renamed
-- import XMonad.Layout.ResizableTile
-- import XMonad.Layout.Spiral
-- import XMonad.Layout.StackTile
-- import XMonad.Layout.Tabbed
-- import XMonad.Layout.TwoPane

-- import XMonad.Config.Desktop

import Data.Map qualified as M
import Data.Monoid (Endo)
import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_AudioLowerVolume,
    xF86XK_AudioMute,
    xF86XK_AudioNext,
    xF86XK_AudioPlay,
    xF86XK_AudioPrev,
    xF86XK_AudioRaiseVolume,
    xF86XK_MonBrightnessDown,
    xF86XK_MonBrightnessUp,
  )
import System.IO ()
import XMonad
  ( Default (def),
    Dimension,
    KeyMask,
    KeySym,
    Mirror (Mirror),
    Query,
    Tall (Tall),
    WindowSet,
    X,
    XConfig
      ( borderWidth,
        focusedBorderColor,
        handleEventHook,
        layoutHook,
        logHook,
        manageHook,
        modMask,
        normalBorderColor,
        terminal,
        workspaces, startupHook
      ),
    appName,
    className,
    composeAll,
    doFloat,
    doIgnore,
    doShift,
    mod4Mask,
    sendMessage,
    shiftMask,
    spawn,
    stringProperty,
    title,
    windows,
    xK_0,
    xK_Left,
    xK_Print,
    xK_Right,
    xK_Tab,
    xK_a,
    xK_e,
    xK_f,
    xK_grave,
    xK_i,
    xK_l,
    xK_m,
    xK_n,
    xK_o,
    xK_p,
    xK_r,
    xK_s,
    xK_t,
    xK_v,
    xK_w,
    xK_x,
    xK_y,
    xK_z,
    xmonad,
    (-->),
    (.|.),
    (<+>),
    (=?),
    (|||),
  )
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Actions.CycleWS (Direction1D (Next, Prev), WSType (Not, WSIs), emptyWS, moveTo, nextScreen, swapNextScreen, toggleWS')
import XMonad.Actions.Submap (submap)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, ppCurrent, ppLayout, ppOutput, ppSep, ppSort, ppTitle, ppUrgent, ppVisible, ppWsSep, shorten, wrap, xmobarColor, xmobarPP, xmobarProp, xmobarStrip, PP (ppExtras, ppHidden))
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, setEwmhActivateHook)
import XMonad.Hooks.InsertPosition (Focus (Newer), Position (Master), insertPosition)
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, checkDock, docks, manageDocks)
import XMonad.Hooks.ManageHelpers
  ( doFullFloat,
    doLower,
    isFullscreen,
  )
import XMonad.Hooks.StatusBar
  ( defToggleStrutsKey,
    statusBarProp,
    withEasySB,
  )
import XMonad.Hooks.StatusBar.PP
  ( PP
      ( ppCurrent,
        ppLayout,
        ppSep,
        ppSort,
        ppTitle,
        ppUrgent,
        ppVisible,
        ppWsSep
      ),
    shorten,
    wrap,
    xmobarColor,
    xmobarPP,
    xmobarStrip,
  )
import XMonad.Hooks.UrgencyHook
  ( NoUrgencyHook (NoUrgencyHook),
    doAskUrgent,
    focusUrgent,
    withUrgencyHook,
  )
import XMonad.Layout.Accordion ()
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.MosaicAlt ()
import XMonad.Layout.MultiToggle
  ( EOT (EOT),
    Toggle (Toggle),
    mkToggle,
    (??),
  )
import XMonad.Layout.MultiToggle.Instances
  ( StdTransformers (FULL, NOBORDERS),
  )
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Spacing (Spacing, smartSpacing)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.StackSet qualified as W
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Hacks qualified as Hacks
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
    scratchpadWorkspaceTag,
  )
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare (filterOutWs)
import Text.Printf (printf)
import Data.Maybe (isJust)

-- import XMonad.Operations (killWindow)

main :: IO ()
main =
  xmonad $
    docks $
      withUrgencyHook NoUrgencyHook $
        setEwmhActivateHook doAskUrgent . ewmh $
            def
              { borderWidth = myBorderWidth,
                focusedBorderColor = myFocusedBorderColor,
                handleEventHook = myEventHooks,
                layoutHook =
                  smartBorders . avoidStruts $
                    mkToggle
                      (NOBORDERS ?? FULL ?? EOT)
                      myLayoutHook,
                logHook = dynamicLogWithPP myPP <+> updatePointer (0.5, 0.5) (0, 0),
                manageHook =
                  manageDocks
                    <+> insertPosition Master Newer
                    <+> myManageHook
                    <+> namedScratchpadManageHook scratchpads
                    <+> manageHook def,
                startupHook = do
                  spawnOnce "eww daemon && eww open main",
                modMask = superKey,
                normalBorderColor = myNormalBorderColor,
                terminal = myTerminal,
                workspaces = myWorkspaces
              }
              `additionalKeys` myKeys

nspWorkspaces = ["NSP"]
filterNSP = unwords . filter (`notElem` nspWorkspaces) . words

appendToNamedPipe namedPipe str = do
    let filteredStr = filterNSP str
    appendFile namedPipe $ wrap "(box :spacing 10 :orientation \"h\" :class \"xmonad\" :halign \"center\" :valign \"center\" :vexpand \"true\" :hexpand \"true\" " ")" filteredStr

formatHiddenWs wsName =
  if wsName /= "NSP"
  then buildWsButton "hidden" wsName
  else wsName

wrapWithEventBox number label = "(eventbox :cursor \"pointer\" :onclick \"wmctrl -s "++ number ++ "\" "++ label ++")"

buildWsButton className wsName =
  case getParts wsName of
    [number, text] -> wrapWithEventBox number $ "(label :class \"workspace "++ className ++"\" :text \""++ text ++"\")"
    _ -> ""


getParts = splitString ':'
  where
    splitString :: Char -> String -> [String]
    splitString _ "" = []
    splitString delimiter input = go input []
      where
        go "" acc = [reverse acc]
        go (c:cs) acc
          | c == delimiter = reverse acc : go cs []
          | otherwise = go cs (c : acc)

myPP =
  def
    { ppOutput = appendToNamedPipe "/home/juboba/wsinfo"
    , ppCurrent = buildWsButton "current"
    , ppVisible = buildWsButton "visible"
    -- , ppTitle = printf "(label :class \"window-title\" :text \"%s\")"
    , ppTitle = const ""
    , ppWsSep = " "
    , ppUrgent = buildWsButton "urgent"
    , ppLayout = printf "(label :class \"layout\" :text \"%s\")"
    , ppHidden = formatHiddenWs
    , ppSep = " "
    }

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
scratchpads = [NS "terminal" "alacritty --class scratch-term -e jmux" (appName =? "scratch-term") (customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4))]

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
myWorkspaces =
  addIndexes [ "\61728" -- 
  , "\61508" -- 
  , "\62038" -- 
  , "\62056" -- 
  , "\61574" -- 
  , "\61477" -- ''
  , "\61664" -- 
  , "\61760" -- 
  , "\62057" -- 
  ]
  where addIndexes ws = [show n ++ ":" ++ w | (index, w) <- zip [0..8] ws, let n = index]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor :: String
myNormalBorderColor = "#000000"

myFocusedBorderColor :: String
myFocusedBorderColor = "#30bced"

goFull :: X ()
goFull = do
  sendMessage $ Toggle FULL
  sendMessage ToggleStruts

------------------------------------------------------------------------
myEventHooks = Hacks.trayerAboveXmobarEventHook <> Hacks.windowedFullscreenFixEventHook <> Hacks.trayerPaddingXmobarEventHook

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
-- myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
--
nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  -- Toggle Fullscreen
  [ ((superKey, xK_f), goFull),
    ((superKey .|. shiftMask, xK_f), sendMessage ToggleStruts),
    ((superKey, xK_Left), moveTo Prev nonEmptyNonNSP),
    ((superKey, xK_Right), moveTo Next nonEmptyNonNSP),
    -- Go to previous workspace
    ((superKey, xK_Tab), toggleWS' ["NSP"]),
    -- , ((superKey, xK_g), goToSelected defaultConfig)
    -- Cycle tiling modes
    -- , ((superKey, xK_0), sendMessage NextLayout)
    -- Send current workspace to next screen
    ((superKey .|. shiftMask, xK_o), swapNextScreen >> nextScreen),
    ((superKey .|. shiftMask, xK_e), launchVim),
    ((superKey .|. shiftMask, xK_w), spawn "rofisxiv"),
    -- Focus next screen
    ((superKey, xK_o), nextScreen),
    ((superKey, xK_s), windows copyToAll), -- @@ Make focused window always visible
    ((superKey .|. shiftMask, xK_s), killAllOtherCopies), -- @@ Toggle window state back
    -- Focus urgent window
    ((superKey, xK_x), focusUrgent),
    -- Kill window
    ((superKey .|. shiftMask, xK_0), spawn "xkill"),
    -- Suspend
    ((superKey .|. shiftMask, xK_x), spawn "systemctl suspend"),
    -- Select sound card
    ((superKey, xK_v), spawn "rofi -show sndsel -modes \"sndsel:sndsel\""),
    -- Invert screen colors
    -- , ((superKey .|. shiftMask, xK_v), spawn "invert_colors")
    -- Screensaver
    ((superKey .|. shiftMask, xK_z), spawn "slock"),
    -- Copy Emoji
    ((superKey .|. shiftMask, xK_i), spawn "rofiemoji"),
    ( (superKey, xK_m),
      submap . M.fromList $
        [ ((0, xK_n), spawn "sp next"),
          ((shiftMask, xK_n), spawn "sp prev"),
          ((0, xK_p), spawn "sp play")
        ]
    ),
    -- Clipboard Menu
    ((superKey .|. shiftMask, xK_m), spawn "clipmenu"),
    -- Launch color picker
    -- , ((superKey .|. shiftMask, xK_y), spawn "pick-colour-picker")
    -- Set slack status
    ((superKey .|. shiftMask, xK_l), spawn "slack-do"),
    -- Select screen
    ((superKey, xK_p), spawn "rofi -show smod -modes \"smod:smod\""),
    -- Toggle terminal scratchpad
    ((superKey .|. shiftMask, xK_t), namedScratchpadAction scratchpads "terminal"),
    -- Launch Screenshot
    ((0, xK_Print), spawn "flameshot gui"),
    -- Launch Volatile Screenshot
    ((shiftMask, xK_Print), spawn "sleep 1; sshot -t"),
    -- Media keys
    ((0, xF86XK_AudioMute), spawn "volume-control mute notify"),
    ((0, xF86XK_AudioRaiseVolume), spawn "volume-control up notify"),
    ((0, xF86XK_AudioLowerVolume), spawn "volume-control down notify"),
    ((0, xF86XK_AudioPlay), spawn "playerctl --player=spotify play-pause"),
    ((0, xF86XK_AudioPrev), spawn "playerctl --player=spotify previous"),
    ((0, xF86XK_AudioNext), spawn "playerctl --player=spotify next"),
    ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl s 10%+"),
    ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10%-"),
    -- , ((shiftMask, xF86XK_KbdBrightnessUp), spawn "shine temp-up")
    -- , ((shiftMask, xF86XK_KbdBrightnessDown), spawn "shine temp-down")
    -- Launch rofi
    ((superKey, xK_e), spawn myLauncher),
    -- Toggle window transparency
    ((superKey, xK_r), spawn "transset 0.8 -t -a"),
    -- Notifications
    ((superKey .|. shiftMask, xK_n), spawn "dunstctl set-paused toggle"),
    ((superKey, xK_grave), spawn "dunstctl close-all"),
    ((superKey .|. shiftMask, xK_grave), spawn "dunstctl history-pop"),
    ((superKey .|. shiftMask, xK_a), spawn "dunstctl action"),
    ((superKey .|. shiftMask, xK_y), spawn "toggle_picom")
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
myManageHook =
  composeAll
    [ isFullscreen --> doFullFloat
    , appName =? "chromium-browser (dev-profile)" --> takeTo 2
    , appName =? "chromium-browser" --> takeTo 3
    , appName =? "google-chrome" --> takeTo 3
    , appName =? "plasmashell" --> doIgnore -- Plasma stuff
    , appName =? "showmyself" --> doFloat -- Show me
    , checkDock --> doLower
    , className =? ".pick-colour-picker-wrapped" --> doFloat -- Color picker
    , className =? "Cypress" --> takeTo 7
    , className =? "Emacs" --> takeTo 1
    , className =? "GSH" --> takeTo 7
    , className =? "Gsimplecal" --> doFloat -- Calendar window
    , className =? "KotatogramDesktop" --> takeTo 4
    , className =? "Pavucontrol" --> doFloat
    , className =? "Peek" --> doFloat
    , className =? "Slack" --> takeTo 4
    , className =? "Spotify" --> takeTo 5
    , className =? "Xmessage" --> doFloat
    , className =? "discord" --> takeTo 4
    , className =? "firefox" --> takeTo 8
    , className =? "flameshot" --> doIgnore
    , className =? "qutebrowser" --> takeTo 8
    , className =? "trayer" --> doIgnore
    , stringProperty "_NET_WM_STATE(ATOM)" =? "_NET_WM_STATE_SKIP_TASKBAR" --> doIgnore
    , title =? "Copying Files" --> doFloat
    , title =? "Media viewer" --> doFloat -- Telegram media
    , title =? "Picture-in-Picture" --> doFloat -- Firefox videos
    , title =? "meet.google.com is sharing your screen." --> takeTo 6
    -- , resource  =? "desktop_window"                                     --> doIgnore
    -- , title =? "Slack - Huddle"                                            --> killWindow
    ]
  where
    takeTo n = doShift $ myWorkspaces !! n

-- where viewShift = doF . liftM2 (.) W.greedyView W.shift

------------------------------------------------------------------------
addGap = smartSpacing 10

-- Layouts:
mainLayout :: ModifiedLayout Spacing Tall a
mainLayout = addGap $ Tall nmaster delta ratio
  where
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2

-- tallLayout :: ModifiedLayout
tallLayout = named "\61659" $ avoidStruts mainLayout -- 

wideLayout = named "\61785" $ avoidStruts $ Mirror mainLayout -- 

messagingLayout = named "\61659" $ addGap $ Tall nmaster delta ratio
  where
    nmaster = 1
    delta = 3 / 100
    ratio = 4 / 5

threeColumns = named "3" $ addGap $ ThreeColMid 1 (3 / 100) (1 / 2)

-- myLayoutHook :: ModifiedLayout ??
myLayoutHook =
  onWorkspace
    (myWorkspaces !! 4)
    messagingLayout
    tallLayout
    ||| wideLayout
    ||| threeColumns
