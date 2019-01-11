import XMonad

import XMonad.Actions.WithAll
import XMonad.Actions.CycleWS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad

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
import XMonad.Layout.Named
import XMonad.Layout.Master
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows
import XMonad.Layout.Simplest

import System.IO

import qualified XMonad.StackSet as W

import qualified Data.Map as M

myMask = mod4Mask -- win key

------------------------------------------------------------------------}}}
-- Colors, Fonts, & Themes                                              {{{
---------------------------------------------------------------------------

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

gap     = 5
border  = 3

myNormalBorderColor  = base03
myFocusedBorderColor = active

active       = cyan
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02

myFont     = "xft:Input:pixelsize=20:bold:antialias=true:hinting=true"
myBigFont  = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"

topBarTheme = def
    { fontName            = myFont
    , inactiveBorderColor = base03
    , inactiveColor       = base03
    , inactiveTextColor   = base03
    , activeBorderColor   = active
    , activeColor         = active
    , activeTextColor     = active
    , urgentBorderColor   = red
    , urgentTextColor     = yellow
    , decoHeight          = 5
    }

myTabTheme = def
    { fontName            = myFont
    , activeColor         = green
    , inactiveColor       = base02
    , activeBorderColor   = green
    , inactiveBorderColor = base02
    , activeTextColor     = base03
    , inactiveTextColor   = base1
    , decoHeight          = 30
    }

------------------------------------------------------------------------}}}
-- Applications & Utilities                                             {{{
---------------------------------------------------------------------------

myTerminal           = "alacritty"
myScratchpadTerminal = "urxvt"
myStatusBar          = "xmobar -x0 -o ~/.xmonad/xmobar.conf"

myFocusFollowsMouse  = False
myClickJustFocuses   = True
myPlacement          = fixed (0.5, 0.5) -- center of the screen

myWorkspaces = map show [1 .. 9 :: Int]

myManageHook =
    composeAll
        [ manageScratchPad
        , placeHook myPlacement
        , isFullscreen --> doFullFloat
        , name =? "discord" --> doShift "3"
        , className =? "Steam" --> doShift "2"
        , className =? "Pavucontrol" --> doFloat
        , className =? "Seahorse" --> doFloat
        , role =? "gimp-toolbox-color-dialog" --> doFloat
        , composeOne [ isFullscreen -?> doFullFloat ]
        , manageDocks
        ]
    where
        name = stringProperty "WM_NAME"
        role = stringProperty "WM_WINDOW_ROLE"

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
    where
        d = 1
        g = 5
        h = (g - 2 * d) / g
        w = (g - 2 * d) / g
        t = d / g
        l = d / g

myLayout =
    windowNavigation $
    smartBorders .
    mkToggle (NOBORDERS ?? FULL ?? EOT) $
    tiled ||| tabs ||| grid ||| bsp ||| masterTabbed
    where
        myGaps  = smartSpacingWithEdge gap

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

        masterTabbed = named "Master-Tabbed Wide"
            $ avoidStruts
            $ gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, 0)]
            $ mastered (1/100) (2/3)
            $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
            $ tabbed shrinkText myTabTheme

myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                                         >> windows W.shiftMaster)
    -- mod-button2 %! Raise the window to the top of the stack
    , (( modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , (( modMask, button3), \w -> focus w >> mouseResizeWindow w
                                         >> windows W.shiftMaster)
    , (( modMask, button4 ), const prevWS)
    , (( modMask, button5 ), const nextWS)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myKeys =
    [ ((myMask, xK_F2), spawn "inox --force-device-scale-factor=1.5") -- Launch browser
      -- Applications menu
    , ((myMask, xK_Tab), spawn "rofi -show combi")
      -- Kill focused
    , ((myMask, xK_BackSpace), kill)
      -- Kill all
    , ((myMask .|. shiftMask, xK_BackSpace), killAll)
      -- Launch a terminal
    , ((myMask, xK_Return), spawn myTerminal)
      -- Swap the focused window and the master window
    , ((myMask .|. shiftMask, xK_Return), windows W.swapMaster)
      -- Manage scratchpad
    , ((myMask, xK_minus), scratchPad)
      -- Calculator
    , ((myMask, xK_c), spawn $ myTerminal ++ " -e bc -l")
      -- Mutt
    , ((myMask, xK_m), mail)
      -- Toggle fullscreen
    , ((myMask, xK_f), sendMessage $ Toggle FULL)
      -- Recompile and restart xmonad
    , ((myMask, xK_0), spawn "i3lock -n")
    , ((myMask, xK_q), spawn "xmonad --recompile && xmonad --restart")

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
    ]
    where
        scratchPad = scratchpadSpawnActionTerminal myScratchpadTerminal
        mail = spawn $ myTerminal ++ " -e neomutt"

myLogHook h =
    dynamicLogWithPP $ def
        { ppCurrent         = xmobarColor active "" . wrap "[" "]"
        , ppTitle           = xmobarColor active "" . shorten 30
        , ppVisible         = wrap "(" ")"
        , ppUrgent          = xmobarColor red "" . wrap " " " "
        , ppHiddenNoWindows = const ""
        , ppSep             = xmobarColor red blue " : "
        , ppWsSep           = " "
        , ppLayout          = xmobarColor yellow ""
        , ppOrder           = id
        , ppOutput          = hPutStrLn h
        , ppExtras          = []
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
