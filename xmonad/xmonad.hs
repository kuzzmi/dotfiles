import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Grid

import System.IO

import qualified XMonad.StackSet as W

myTerminal = "alacritty"

myManageHook =
    composeAll
        [ manageScratchPad
        ] <+> manageHook def

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 0.1     -- terminal height, 10%
        w = 1       -- terminal width, 100%
        t = 1 - h   -- distance from top edge, 90%
        l = 1 - w   -- distance from left edge, 0%

myLayout =
    mkToggle (NOBORDERS ?? FULL ?? EOT)
    $ Grid ||| tiled ||| Mirror tiled
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2
        -- Percent of screen to increment by when resizing panes
        delta   = 2/100

myKeys =
    -- Launch browser
    [ ((mod4Mask, xK_F2), spawn "inox --force-device-scale-factor=1.5")
    -- Applications menu
    , ((mod4Mask, xK_Tab), spawn "rofi -show combi")
    -- Kill focused
    , ((mod4Mask, xK_BackSpace), kill)
    -- Launch a terminal
    , ((mod4Mask, xK_Return), spawn myTerminal)
    -- Swap the focused window and the master window
    , ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)
    -- Manage scratchpad
    , ((mod4Mask, xK_minus), scratchPad)
    -- Toggle fullscreen
    , ((mod4Mask, xK_f), sendMessage $ Toggle FULL)
    -- Recompile and restart xmonad
    , ((mod4Mask, xK_q), spawn "stack ghc -v ~/.xmonad/xmonad.hs; ~/.xmonad/xmonad --restart")
    ]
    where
        scratchPad = scratchpadSpawnActionTerminal ("xterm --title=scratchpad")

main = do
    xmonad $ def
        { terminal    = myTerminal
        , modMask     = mod4Mask
        , manageHook  = myManageHook
        , layoutHook  = myLayout
        } `additionalKeys` myKeys
