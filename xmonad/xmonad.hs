import XMonad

import XMonad.Actions.WithAll
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.GridSelect

import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad
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

myMask = mod4Mask -- win key

------------------------------------------------------------------------}}}
-- Colors, Fonts, & Themes                                              {{{
---------------------------------------------------------------------------

-- base03  = "#002b36"
-- base02  = "#073642"
-- base01  = "#586e75"
-- base00  = "#657b83"
-- base0   = "#839496"
-- base1   = "#93a1a1"
-- base2   = "#eee8d5"
-- base3   = "#fdf6e3"
-- yellow  = "#b58900"
-- orange  = "#cb4b16"
-- red     = "#dc322f"
-- magenta = "#d33682"
-- violet  = "#6c71c4"
-- blue    = "#268bd2"
-- cyan    = "#2aa198"
-- green   = "#859900"

bg      = "#F3F3F3"
fg      = "#707070"
color00 = "#D3D3D3"
color01 = "#EF6B7B"
color02 = "#A1D569"
color03 = "#F59335"
color04 = "#4EC2E8"
color05 = "#FEC7CD"
color06 = "#95C1C0"
color07 = "#707070"
color08 = "#B3B3B3"
color09 = "#ED5466"
color10 = "#AFDB80"
color11 = "#F59335"
color12 = "#5DC7EA"
color13 = "#D2A4B4"
color14 = "#75A1A0"
color15 = "#909090"

gap     = 20
border  = 5

myNormalBorderColor  = inactive
myFocusedBorderColor = active

active   = color04
inactive = color07
urgent   = color09

myFont     = "xft:JetBrainsMono:weight:900:pixelsize=20:bold:antialias=true:hinting=true"
myBigFont  = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"

myTabTheme = def
    { fontName            = myFont
    , activeColor         = active
    , inactiveColor       = inactive
    , activeBorderColor   = active
    , inactiveBorderColor = inactive
    , activeTextColor     = inactive
    , inactiveTextColor   = active
    , decoHeight          = 40
    }

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
    where
        navKeyMap = M.fromList
            [ ((0, xK_Escape), cancel)
            , ((0, xK_space), select)
            , ((0, xK_Return), select)
            , ((0, xK_slash) , substringSearch myNavigation)
            , ((0, xK_h)     , move (-1,0)  >> myNavigation)
            , ((0, xK_Left)  , move (-1,0)  >> myNavigation)
            , ((0, xK_l)     , move (1,0)   >> myNavigation)
            , ((0, xK_Right) , move (1,0)   >> myNavigation)
            , ((0, xK_j)     , move (0,1)   >> myNavigation)
            , ((0, xK_Down)  , move (0,1)   >> myNavigation)
            , ((0, xK_Up)    , move (0,-1)  >> myNavigation)
            , ((0, xK_k)     , move (0,-1)  >> myNavigation)
            , ((0, xK_0) , setPos (0,0) >> myNavigation)
            ]
        navDefaultHandler = const myNavigation


myGSTheme = def
    { gs_font = myFont
    , gs_cellwidth = 200
    , gs_cellheight = 50
    , gs_navigate = myNavigation
    }

------------------------------------------------------------------------}}}
-- Applications & Utilities                                             {{{
---------------------------------------------------------------------------

myTerminal           = "alacritty"
myScratchpadTerminal = "urxvt"
myStatusBar          = "xmobar -x0 -o ~/.xmonad/xmobar.conf"
myBrowser            = "chromium"

myFocusFollowsMouse  = False
myClickJustFocuses   = False
myPlacement          = fixed (0.5, 0.5) -- center of the screen

myWorkspaces = map show [1 .. 9 :: Int]

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
        , role =? "gimp-toolbox-color-dialog" --> doFloat
        , role =? "gimp-message-dialog" --> doFloat
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
        , NS "email" "thunderbird" (className =? "Thunderbird") (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
        ]

myLayout =
    windowNavigation $
    smartBorders .
    onWorkspace "3" tabs .
    full $
    tiled ||| tabs ||| grid ||| bsp
    where
        myGaps = smartSpacingWithEdge gap

        full = mkToggle (NOBORDERS ?? FULL ?? EOT)

        nmaster = 1
        ratio   = 1 / 2
        delta   = 1 / 15

        bsp = named "BSP"
            $ avoidStruts
            $ myGaps emptyBSP

        grid = named "Grid"
            $ avoidStruts
            $ myGaps Grid

        tiled = named "Tiled"
            $ avoidStruts
            $ myGaps
            $ Tall nmaster delta ratio

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
    , ((myMask, xK_Tab), spawn "rofi -show combi")

      -- Kill focused
    , ((myMask, xK_BackSpace), kill)

      -- Kill all
    , ((myMask .|. shiftMask, xK_BackSpace), killAll)

      -- Launch a terminal
    , ((myMask, xK_Return), spawn myTerminal)

      -- Launch a terminal
    , ((myMask .|. controlMask, xK_Return), spawn "alacritty -e ranger")

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
    , ((0, xK_Print), spawn "scrot")

      -- Run pavucontrol
    , ((myMask .|. shiftMask, xK_m), spawn "pavucontrol")

    , ((myMask, xK_Left), prevWS)
    , ((myMask, xK_Right), nextWS)

    , ((myMask .|. controlMask, xK_h), sendMessage $ pullGroup L)
    , ((myMask .|. controlMask, xK_l), sendMessage $ pullGroup R)
    , ((myMask .|. controlMask, xK_k), sendMessage $ pullGroup U)
    , ((myMask .|. controlMask, xK_j), sendMessage $ pullGroup D)

    , ((myMask .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    , ((myMask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

    , ((myMask .|. controlMask, xK_period), onGroup W.focusDown')
    , ((myMask .|. controlMask, xK_comma), onGroup W.focusUp')

    , ((myMask, xK_grave), goToSelected myGSTheme)
    ]

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

myConfig p = docks $ def
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
