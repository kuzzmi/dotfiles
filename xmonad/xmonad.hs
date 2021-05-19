import XMonad

import XMonad.Actions.WithAll
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS

import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Decoration
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Named
import XMonad.Layout.Master
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows
import XMonad.Layout.Simplest
import XMonad.Layout.PerWorkspace

import System.IO

import qualified XMonad.StackSet as W

import qualified Data.Map as M

import Data.List(isInfixOf)

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

myMask = mod4Mask -- win key

------------------------------------------------------------------------}}}
-- Colors, Fonts, & Themes                                              {{{
---------------------------------------------------------------------------

bg      = "#2d2d2d"
fg      = "#d3d0c8"
color00 = "#2d2d2d"
color01 = "#f2777a"
color02 = "#99cc99"
color03 = "#ffcc66"
color04 = "#6699cc"
color05 = "#cc99cc"
color06 = "#66cccc"
color07 = "#d3d0c8"
color08 = "#2d2d2d"
color09 = "#f2777a"
color10 = "#99cc99"
color11 = "#ffcc66"
color12 = "#6699cc"
color13 = "#cc99cc"
color14 = "#66cccc"
color15 = "#d3d0c8"

gap     = 16
border  = 0

active   = color04
inactive = color00
urgent   = color01

myNormalBorderColor  = inactive
myFocusedBorderColor = active

myFont     = "xft:JetBrainsMono:weight:900:pixelsize=22:bold:antialias=true:hinting=true"
myBigFont  = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"

myTabTheme = def
    { fontName            = myFont
    , activeColor         = active
    , inactiveColor       = inactive
    , activeBorderColor   = active
    , inactiveBorderColor = inactive
    , activeTextColor     = inactive
    , inactiveTextColor   = active
    , decoHeight          = 36
    }

------------------------------------------------------------------------}}}
-- Applications & Utilities                                             {{{
---------------------------------------------------------------------------

myTerminal           = "alacritty"
myScratchpadTerminal = "urxvt"
myStatusBar          = "xmobar -x0 -o ~/.xmonad/xmobar.conf"
myBrowser            = "google-chrome-stable"

myFocusFollowsMouse  = False
myClickJustFocuses   = False
myPlacement          = fixed (0.5, 0.5) -- center of the screen

myWorkspaces = map show [1 .. 9 :: Int]

-- Queries for manage hook
q ~? x = fmap (x `isInfixOf`) q
q !? x = fmap (not . isInfixOf x) q

myManageHook =
    composeAll
        [ namedScratchpadManageHook myScratchpads
        , placeHook myPlacement
        , isFullscreen --> doFullFloat

        , name =? "discord" --> doShift "3"
        , className =? "Discord" --> doShift "3"
        , className =? "discord" --> doShift "3"
        , className =? "Slack" --> doShift "3"
        , className =? "Steam" --> doShift "4"
        , className =? "Chromium" --> doShift "2"
        , className =? "Pavucontrol" --> doFloat
        , className =? "Seahorse" --> doFloat
        , className =? "MEGAsync" --> doFloat
        , role ~? "gimp-" <&&> role !? "gimp-image-window" --> doFloat
        , composeOne [ isFullscreen -?> doFullFloat ]
        , manageDocks
        ]
    where
        name = stringProperty "WM_NAME"
        role = stringProperty "WM_WINDOW_ROLE"

-- Scratch Pads
myScratchpads =
        [ NS "telegram" "telegram-desktop" ((className =? "Telegram") <||> (className =? "telegram-desktop") <||> (className =? "TelegramDesktop")) (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
        , NS "terminal" "alacritty -t alacritty-scratch" (title =? "alacritty-scratch") (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
        , NS "email" "mailspring" (className =? "Mailspring") (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
        ]

myLayout =
    windowNavigation $
    smartBorders .
    onWorkspace "3" tabs .
    onWorkspace "4" tabs .
    full $
    bsp ||| tabs
    where
        myGaps = smartSpacingWithEdge gap

        full = mkToggle (NOBORDERS ?? FULL ?? EOT)

        nmaster = 1
        ratio   = 1 / 2
        delta   = 1 / 15

        bsp = named "BSP"
            $ avoidStruts
            $ myGaps emptyBSP

        -- grid = named "Grid"
        --     $ avoidStruts
        --     $ myGaps Grid

        -- tiled = named "Tiled"
        --     $ avoidStruts
        --     $ myGaps
        --     $ Tall nmaster delta ratio

        tabs = named "Tabs"
            $ avoidStruts
            $ tabbedBottom shrinkText myTabTheme

myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                                         >> windows W.shiftMaster)
    -- mod-button2 %! Raise the window to the top of the stack
    , (( modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , (( modMask, button3), \w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster)
    , (( modMask, button4 ), const $ moveTo Prev NonEmptyWS)
    , (( modMask, button5 ), const $ moveTo Next NonEmptyWS)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myKeys =
    [ ((myMask, xK_F2), spawn myBrowser) -- Launch browser

      -- Applications menu
    , ((myMask, xK_grave), spawn "rofi -show combi")

      -- Emoji menu
    , ((myMask, xK_e), spawn "rofi -show emoji")

      -- Kill focused
    , ((myMask, xK_BackSpace), kill)

      -- Kill all
    , ((myMask .|. shiftMask, xK_BackSpace), killAll)

      -- Launch a terminal
    , ((myMask, xK_Return), spawn myTerminal)

      -- Swap the focused window and the master window
    , ((myMask .|. shiftMask, xK_Return), windows W.swapMaster)

      -- Manage scratchpad
    , ((myMask, xK_minus), namedScratchpadAction myScratchpads "terminal")
    , ((myMask, xK_equal), namedScratchpadAction myScratchpads "telegram")
    , ((myMask, xK_m), namedScratchpadAction myScratchpads "email")

      -- Calculator
    , ((myMask, xK_c), spawn "rofi -show calc -modi calc -no-show-match -no-sort")

      -- Toggle fullscreen
    , ((myMask, xK_f), sendMessage $ Toggle FULL)

      -- Recompile and restart xmonad
    , ((myMask, xK_0), spawn "i3lock -n -c 000000")
    , ((myMask, xK_q), spawn "xmonad --recompile && xmonad --restart")

    , (( myMask, xK_b ), cycleRecentWS [xK_b] xK_b xK_b ) -- previous workspace, like in i3

    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((controlMask .|. shiftMask, xK_Print), spawn "sleep 0.2; scrot -s /tmp/screen.png; xclip -selection clipboard -t image/png -i /tmp/screen.png; rm /tmp/screen.png")
    , ((shiftMask, xK_Print), spawn "scrot /tmp/screen.png; xclip -selection clipboard -t image/png -i /tmp/screen.png; rm /tmp/screen.png")
    , ((0, xK_Print), spawn "flameshot gui")

      -- Run pavucontrol
    , ((myMask .|. shiftMask, xK_m), spawn "pavucontrol")

      -- My own version of the mount script
    , ((myMask, xK_F12), spawn "rofi-mount")

    , ((myMask, xK_Left), prevWS)
    , ((myMask, xK_Right), nextWS)

      -- Audio
    , ((myMask, xK_Page_Up), spawn "/home/kuzzmi/.xmonad/xmonad-pulsevolume/pulse-volume.sh increase")
    , ((myMask, xK_Page_Down), spawn "/home/kuzzmi/.xmonad/xmonad-pulsevolume/pulse-volume.sh decrease")
    , ((myMask, xK_End), spawn "/home/kuzzmi/.xmonad/xmonad-pulsevolume/pulse-volume.sh toggle")
    , ((myMask, xK_Home), spawn "/home/kuzzmi/.xmonad/xmonad-pulsevolume/pulse-volume.sh reset")

    , ((myMask, xK_x), sendMessage $ ToggleGaps)
    , ((myMask, xK_z), withFocused centerWindow)

    -- , ((myMask .|. controlMask, xK_h), sendMessage $ pullGroup L)
    -- , ((myMask .|. controlMask, xK_l), sendMessage $ pullGroup R)
    -- , ((myMask .|. controlMask, xK_k), sendMessage $ pullGroup U)
    -- , ((myMask .|. controlMask, xK_j), sendMessage $ pullGroup D)
    --
    -- , ((myMask .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    -- , ((myMask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

    -- , ((myMask .|. controlMask, xK_period), onGroup W.focusown')
    -- , ((myMask .|. controlMask, xK_comma), onGroup W.focusUp')
    ]
    where
    centerWindow :: Window -> X ()
    centerWindow win = do
        (_, W.RationalRect x y w h) <- floatLocation win
        windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)
        return ()

myLogHook h =
    dynamicLogWithPP $ def
        { ppCurrent         = xmobarColor active ""
        , ppTitle           = xmobarColor active "" . shorten 30
        , ppVisible         = wrap "(" ")"
        , ppUrgent          = xmobarColor urgent "" . wrap "!" "!"
        , ppHiddenNoWindows = const ""
        , ppWsSep           = " "
        , ppLayout          = xmobarColor color11 ""
        , ppOrder           = id
        , ppOutput          = hPutStrLn h
        , ppExtras          = []
        , ppSort            = fmap (.namedScratchpadFilterOutWorkspace) getSortByTag
        }

main = do
    xmproc <- spawnPipe myStatusBar
    xmonad $ myConfig xmproc

myConfig p = docks $ withUrgencyHook LibNotifyUrgencyHook $ ewmh def
    { borderWidth        = border
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , terminal           = myTerminal
    , modMask            = myMask
    , manageHook         = myManageHook
    , layoutHook         = myLayout
    , logHook            = myLogHook p
    , clickJustFocuses   = myClickJustFocuses
    , focusFollowsMouse  = myFocusFollowsMouse
    , workspaces         = myWorkspaces
    , mouseBindings      = myMouseBindings
    } `additionalKeys`
    myKeys
