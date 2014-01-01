
xfce+xmonad in xubuntu
======================

Dependencies
------------
sudo apt-get install libglib2.0-dev libdbus-glib-1-dev xfce4-panel-dev

### DBus:
sudo apt-get install libxml2-dev
cabal update
cabal install dbus

xmonad-log-applet
-----------------
Get xmonad-log-applet: http://kojevnikov.com/xmonad-log-applet-gnome-xfce.html
Grab the tarball, `tar -zxvf` it, do

```
./configure --with-panel=xfce4
make
sudo make install
```

Then just add the XMonad log applet to your panel.

xmonad.hs
---------
Grab the sample xmonad.hs from https://github.com/alexkay/xmonad-log-applet/blob/master/xmonad.hs and replace:
`import XMonad.Config.Gnome` with `import XMonad.Config.Xfce`
and
`xmonad $ gnomeConfig` with `xmonad $ xfceConfig`

Then merge with your xmonad.hs or just grab xmonad.hs from this repo and start from there


