xfce+xmonad in xubuntu
======================

Dependencies
------------
```
sudo apt-get install libglib2.0-dev libdbus-glib-1-dev xfce4-panel-dev xmonad cabal-install
```

### DBus:
```
sudo apt-get install libxml2-dev
cabal update
cabal install dbus
```

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
Xmonad is configured through ~/.xmonad/xmonad.hs

##### If you have your own xmonad.hs and wish to add xfce capabilities

Grab the sample xmonad.hs from https://github.com/alexkay/xmonad-log-applet/blob/master/xmonad.hs and replace:
`import XMonad.Config.Gnome` with `import XMonad.Config.Xfce`
and
`xmonad $ gnomeConfig` with `xmonad $ xfceConfig`

Then merge with your xmonad.hs.

##### If you're starting from scratch or wish to try out my xmonad.hs

Just grab xmonad.hs from this repo and start from there.

Note that I've bound a key to open dmenu, which will obviously only work with dmenu installed:
```
sudo apt-get install suckless-tools
```

Setting up xfce
---------------
Follow the steps under "Configuring XMonad to work with Xfce" to set up xfce: http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_XFCE

That should be all.

non-xfce (xmobar branch)
------------------------
Install xmonad, xmonad-contrib and xmobar with cabal with something like:

```
cabal install --global xmonad-0.12
cabal install --global xmonad-contrib-0.12
cabal install --global -fwith_xft xmobar-0.24.3
```

Install session files from ubuntu-session-files.

To make scratchpad work: put .Xresources in ~/ and run ```xrdb -merge ~/.Xresources```
