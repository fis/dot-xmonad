What's this?
============

This is just my personal `~/.xmonad/` directory.

Overview
--------

There's the XMonad `xmonad.hs` file; that's the XMonad configuration
and/or executable.  For the most part, it's very conventional.  The
only uncommon part is how it's tied to the dzen2-based status bar
system.

The XMonad configuration causes XMonad to send status updates as DBus
signals.  It will also listen for `XMONAD_SWITCH`-containing X "client
message" events; these are used to send workspace-switch commands
back.

The other application, `dzen2-update`, will spawn a separate `dzen2`
status bar for each (Xinerama) screen.  It will listen for the XMonad
status updates, parse those, and update the status bar contents
appropriately.  Clicking the status bar workspace names will also
cause it to send the special workspace-switch events back to XMonad.

How do I use it?
----------------

Well, if you are sure you *want* to...

Personally I use this on some Ubuntu systems, integramated with Gnome,
with a single (bottom of one screen) gnome-panel instance in place to
contain all those fancy and modern indicator applets, tray midgets,
and whatnot.  For that sort of setup, you'd want to install
`libghc6-xmonad-dev` and `libghc6-xmonad-contrib-dev` from the Ubuntu
repositories; that makes it easier to hook it up to Gnome (pre-made
.desktop files and all that).  Also install `cabal-install`; then
`cabal install` the packages `dbus-core`, `regex-posix` and `split`.

In addition, you might want to use the SVN version of dzen2 (`svn
checkout http://dzen.googlecode.com/svn/trunk/ dzen`), at least while
the Ubuntu-packaged version does not do Xft fonts.  (Well, either
that, or opt for not using a Xft font; edit `dzen2-update.hs`
accordingly.)

To hook this up as the Gnome window manager, just `gconftool-2 -s
/desktop/gnome/session/required_components/windowmanager xmonad --type
string` and then use a "classic Gnome" sort of session.  It is also
theoretically possible to make a separate Gnome session thing for
XMonad, but I haven't bothered.
