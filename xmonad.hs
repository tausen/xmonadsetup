import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Actions.SpawnOn
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import System.IO

-- toggle border key
import XMonad.Actions.NoBorders

-- allow chrome fullscreen
import XMonad.Hooks.EwmhDesktops

myWorkspaces = ["web","code","test","im","term","6","7","8","9"]

myManageHook = composeAll
       [ className =? "Gimp"   --> doFloat
       , className =? "Tomboy" --> doFloat
       , className =? "Gbase"  --> doFloat
       , className =? "Main.py" --> doFloat -- guake
       , className =? "Top_block.py" --> doFloat -- gnuradio
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

main = do
    xmproc <- spawnPipe "~/.cabal/bin/xmobar ~/.xmonad/xmobar.hs"

    xmonad $ defaultConfig
        { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , workspaces = myWorkspaces
	, startupHook = setWMName "LG3D" -- matlab fix
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
        ]
