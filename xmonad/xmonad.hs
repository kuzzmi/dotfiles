import XMonad

import XMonad.Actions.WithAll

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad

import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Decoration
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows
import XMonad.Layout.Simplest

import System.IO

import qualified XMonad.StackSet as W

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

gap     = 10
border  = 3

myNormalBorderColor  = base03
myFocusedBorderColor = active

active       = cyan
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02

myFont  = "xft:Input:pixelsize=14:regular:antialias=true:hinting=true"
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
    , activeColor         = active
    , inactiveColor       = base02
    , activeBorderColor   = active
    , inactiveBorderColor = base02
    , activeTextColor     = base03
    , inactiveTextColor   = base1
    , decoWidth           = border * 2
    }

------------------------------------------------------------------------}}}
-- Applications & Utilities                                             {{{
---------------------------------------------------------------------------

myTerminal           = "alacritty"
myScratchpadTerminal = "urxvt"
myStatusBar          = "xmobar -x0 -o"

myFocusFollowsMouse  = False
myClickJustFocuses   = True
myPlacement          = fixed (0.5, 0.5) -- center of the screen

myManageHook =
  composeAll
    [ manageScratchPad
    , placeHook myPlacement
    , isFullscreen --> doFullFloat
    , className =? "Steam" --> doShift "2"
    , className =? "Pavucontrol" --> doFloat
    , className =? "Seahorse" --> doFloat
    , role =? "gimp-toolbox-color-dialog" --> doFloat
    -- , className =? "Gimp" --> doFloat
    ]
  where role = stringProperty "WM_WINDOW_ROLE"

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
    subTabbed $
    boringWindows $
    smartBorders . mkToggle (NOBORDERS ?? FULL ?? EOT) $ tiled ||| tabs ||| grid
    where
        gaps    = smartSpacingWithEdge gap
        grid    = gaps Grid
        tiled   = gaps $ Tall nmaster delta ratio
        tabs    = tabbed shrinkText myTabTheme
        nmaster = 1
        ratio   = 1 / 2
        delta   = 1 / 25

myKeys =
    [ ((myMask, xK_F2), spawn "inox --force-device-scale-factor=1.25") -- Launch browser
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
      -- Mutt
    , ((myMask, xK_m), mail)
      -- Toggle fullscreen
    , ((myMask, xK_f), sendMessage $ Toggle FULL)
      -- Recompile and restart xmonad
    , ((myMask, xK_0), spawn "i3lock -n")
    , ( (myMask, xK_q) , spawn "xmonad --recompile && xmonad --restart")
      -- Run pavucontrol
    , ((myMask .|. controlMask, xK_m), spawn "pavucontrol")

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

myLogHook h = do
    -- following block for copy windows marking
    -- copies <- wsContainingCopies
    -- let check ws | ws `elem` copies =
    --                pad . xmobarColor yellow red . wrap "*" " "  $ ws
    --              | otherwise = pad ws

    -- fadeWindowsLogHook myFadeHook
    -- ewmhDesktopsLogHook
    --dynamicLogWithPP $ defaultPP
    dynamicLogWithPP $ def
        { ppCurrent         = xmobarColor active "" . wrap "[" "]"
        , ppTitle           = xmobarColor active "" . shorten 100
        , ppVisible         = wrap "(" ")"
        , ppUrgent          = xmobarColor red "" . wrap " " " "
        -- , ppHidden       = check
        , ppHiddenNoWindows = const ""
        , ppSep             = xmobarColor red blue " : "
        , ppWsSep           = " "
        , ppLayout          = xmobarColor yellow ""
        , ppOrder           = id
        , ppOutput          = hPutStrLn h
        -- , ppSort         = fmap
        --                    (namedScratchpadFilterOutWorkspace.)
        --                    (ppSort def)
        , ppExtras          = [] }

main = do
    xmproc <- spawnPipe myStatusBar
    xmonad $ myConfig xmproc

myConfig p = def
    { borderWidth        = border
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , terminal           = myTerminal
    , modMask            = myMask
    , manageHook         = myManageHook
    , layoutHook         = myLayout
    , clickJustFocuses   = myClickJustFocuses
    , focusFollowsMouse  = myFocusFollowsMouse
    } `additionalKeys`
  myKeys
