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

These days I use this on Debian without a desktop environment in
place.  If you're curious, see earlier versions for a Gnome-integrated
system.

To use it on Debian, I've installed `libghc-xmonad-dev` and
`libghc-xmonad-contrib-dev` as well as `cabal-install` from the Debian
repositories; then used `cabal install` the packages `dbus`,
`regex-posix`, `split` and `alsa-mixer`.

I also use the latest development version of dzen2; it used to be the
case that released dzen2 versions did not do Xft fonts, though this
may have changed.

The volume control keybindings have been designed for a pure-ALSA
system and might not be very optimal if you have PulseAudio in place.
