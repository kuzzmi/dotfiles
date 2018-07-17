import XMonad

import XMonad.Actions.WithAll

import XMonad.Hooks.Place
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad

import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Grid

import System.IO

import qualified XMonad.StackSet as W

myTerminal           = "alacritty"
myMask               = mod4Mask        -- win key
myScratchpadTerminal = "urxvt"
myPlacement          = fixed (0.5,0.5) -- center of the screen

myManageHook =
    composeAll
        [ placeHook myPlacement
        , className =? "Pavucontrol" --> doFloat
        , className =? "Seahorse"    --> doFloat
        , manageScratchPad
        ]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 1/2 -- terminal height, 50%
        w = 1/2 -- terminal width, 50%
        t = 1/4 -- distance from top edge, 25%
        l = 1/4 -- distance from left edge, 25%

myLayout =
    mkToggle (NOBORDERS ?? FULL ?? EOT)
    $ tiled ||| grid
    where
        gaps    = smartSpacingWithEdge 10
        grid    = gaps $ Grid
        -- default tiling algorithm partitions the screen into two panes
        tiled   = gaps $ Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2
        -- Percent of screen to increment by when resizing panes
        delta   = 1/25

myKeys =
    -- Launch browser
    [ ((myMask, xK_F2), spawn "inox --force-device-scale-factor=1.5")
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
    , ((myMask, xK_q), spawn "stack ghc -v ~/.xmonad/xmonad.hs; ~/.xmonad/xmonad --restart")
    -- Run pavucontrol
    , ((myMask .|. controlMask, xK_m), spawn "pavucontrol")
    ]
    where
        scratchPad = scratchpadSpawnActionTerminal myScratchpadTerminal
        mail       = spawn "alacritty -e neomutt"

main = do
    xmonad $ def
        { terminal    = myTerminal
        , modMask     = myMask
        , manageHook  = myManageHook
        , layoutHook  = myLayout
        } `additionalKeys` myKeys
