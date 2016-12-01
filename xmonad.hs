{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicLog

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect

-- allow chrome fullscreen
import XMonad.Hooks.EwmhDesktops

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

-- toggle border key
import XMonad.Actions.NoBorders

myWorkspaces    = ["web","code","test","im","term","6","7","8","9"]

-- check window class using: xprop | grep -i class

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

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ xfceConfig
         { logHook = dynamicLogWithPP (prettyPrinter dbus)
         , terminal   = "gnome-terminal"
         , modMask    = mod4Mask
         , workspaces = myWorkspaces
         , startupHook =  spawnHere "tomboy"
                          -- >> spawnOn "im" "sleep 8 && hexchat --minimize=2" 
                          -- >> spawnOn "im" "sleep 8 && skype" -- sleeps here to ensure xchat and skype are added to the indicator area
                          -- >> spawnHere "sleep 8 && insync start"
                          -- >> spawnHere "guake"
                          -- couple of key remaps handy with the t430s
                          -- >> spawnHere "/usr/bin/setxkbmap -option 'ctrl:nocaps'" -- remap caps lock to ctrl
                          >> spawnHere "/usr/bin/xmodmap -e 'keycode 107 = Menu'" -- remap print screen to context menu
                          >> setWMName "LG3D" -- matlab fix
         , manageHook = manageDocks <+> manageSpawn <+> myManageHook <+> manageHook xfceConfig
         -- chrome fullscreen
         , handleEventHook = fullscreenEventHook
         }
         `additionalKeysP`
         [ ("M-p", spawn "exe=`dmenu_run` && eval \"exec $exe\""),
           ("M-C-e", spawn "emacsclient -c -a ''"),
           ("M-y", withFocused toggleBorder),
           ("M-g", goToSelected defaultGSConfig) ]

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppVisible  = pangoColor "green" . wrap "(" ")" . pangoSanitize
    , ppCurrent  = pangoColor "yellow" . wrap "[" "]" . pangoSanitize
    , ppHidden   = pangoColor "gray" . pangoSanitize
    , ppUrgent   = pangoColor "red"
    , ppLayout   = pangoColor "gray" . wrap "|" "|" . pangoSanitize
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
