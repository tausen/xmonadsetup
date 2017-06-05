import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Actions.SpawnOn
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W
import System.IO

-- toggle border key
import XMonad.Actions.NoBorders

-- allow chrome fullscreen
import XMonad.Hooks.EwmhDesktops

myWorkspaces = ["web","code","test","im","term","6","7","8","9"]
myTerminal = "gnome-terminal"

myManageHook = composeAll
       [ className =? "Gimp"   --> doFloat
       , className =? "Tomboy" --> doFloat
       , className =? "Gbase"  --> doFloat
       , className =? "Main.py" --> doFloat -- guake
       -- xfce whisker menu
       , className =? "Wrapper-1.0" --> doFloat
       -- automatically move all ipython plot windows to the test workspace (great for dual monitor setups)
       , className =? "Tk" --> doShift "test"
       , className =? "Ipython" --> doShift "test"
       -- prevent xfce notifications from stealing focus
       , className =? "Xfce4-notifyd" --> doIgnore
       , className =? "evolution-alarm-notify" --> doFloat
       , className =? "Evolution-alarm-notify" --> doFloat
       -- fix disappearing popups in Saleae Logic gui
       , title =? "Saleae Logic Software" --> doF (W.shift "5:dls")
       , title =? "Logic" --> doIgnore
       ]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.98
    w = 0.98
    t = 0.01
    l = 0.01

main = do
    xmproc <- spawnPipe "~/.cabal/bin/xmobar /home/tausen/.xmonad/xmobar.hs"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageScratchPad <+> myManageHook <+> manageHook defaultConfig
        , terminal = myTerminal
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , workspaces = myWorkspaces
	, startupHook = setWMName "LG3D" -- matlab fix
                        >> spawnHere "xinput disable 10" -- disable touchpad
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        -- chrome fullscreen
        , handleEventHook = fullscreenEventHook
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "slock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask .|. controlMask, xK_k), spawn "~/shellscripts/setlayout.sh")
        , ((mod4Mask .|. controlMask, xK_e), spawn "emacsclient -c -a ''")
        , ((mod4Mask .|. controlMask, xK_i), spawn "xcalib -invert -alter")
        , ((mod4Mask, xK_s), scratchPad)
        ]
        where
                scratchPad = scratchpadSpawnActionTerminal "xterm"
