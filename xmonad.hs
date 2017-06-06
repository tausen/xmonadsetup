import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Actions.SpawnOn
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
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
       , className =? "Wfica_Seamless" --> doFloat
       -- fix disappearing popups in Saleae Logic gui
       , title =? "Saleae Logic Software" --> doF (W.shift "5:dls")
       , title =? "Logic" --> doIgnore
       ]

myScratchPads = [ NS "mixer" spawnMixer findMixer manageMixer
                , NS "terminal" spawnTerm  findTerm  manageTerm
                ]
  where
    spawnMixer  = "pavucontrol"
    findMixer   = className =? "Pavucontrol"
    manageMixer = customFloating $ W.RationalRect l t w h
      where
        h = 0.95
        w = 0.95
        t = 0.025
        l = 0.025
    spawnTerm   = "xterm -name scratchpad -e tmux"                   -- gnome-terminal seems to ignore --name
    findTerm    = className =? "XTerm"
    manageTerm  = customFloating $ W.RationalRect l t w h
      where
        h = 0.80
        w = 0.90
        t = 0.10
        l = 0.05

main = do
    xmproc <- spawnPipe "~/.cabal/bin/xmobar /home/tausen/.xmonad/xmobar.hs"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> namedScratchpadManageHook myScratchPads <+> myManageHook <+> manageHook defaultConfig
        , terminal = myTerminal
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , workspaces = myWorkspaces
        , startupHook = setWMName "LG3D" -- matlab fix
--                      >> spawnHere "xinput disable 10" -- disable touchpad
--                      >> spawnHere "gsettings set org.gnome.desktop.background picture-uri file:///home/tausen/Pictures/image001.jpg" -- set wallpaper
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
        , ((mod4Mask .|. shiftMask, xK_t), scratchTerm)
        , ((mod4Mask .|. shiftMask, xK_m), scratchMixer)
        , ((mod4Mask, xK_y), withFocused toggleBorder)
        , ((mod4Mask, xK_b), sendMessage ToggleStruts)
        ]
        where
          scratchTerm  = namedScratchpadAction myScratchPads "terminal"
          scratchMixer = namedScratchpadAction myScratchPads "mixer"
