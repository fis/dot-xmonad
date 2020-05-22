What's this?
============

This is just my personal `~/.xmonad/` directory.

Overview
--------

There's the XMonad `xmonad.hs` file; that's the XMonad configuration
and/or executable.  For the most part, it's very conventional.  The
only uncommon part is how it's tied to a custom status bar.

The XMonad configuration causes XMonad to send status updates as DBus
signals.  It will also listen for `XMONAD_SWITCH`-containing X "client
message" events; these are used to send workspace-switch commands
back.

How do I use it?
----------------

Well, if you are sure you *want* to...

These days I use this on Debian unstable, and build with Stack.

In theory all you need to do is to clone the `xmonad` and
`xmonad-contrib` repositories into the `xmonad-git` and
`xmonad-contrib-git` subdirectories. Then call `stack install`.

It'll put the standard `xmonad` binary in your stack `local-bin-path`,
but I hardly ever use that: instead, I use a `.xsession` file to
directly start the `xmonad-$arch-$os` binary. There's also a really
simple build script to make XMonad's recompile action work as
expected.

The custom code resides mostly in the `zem` subdirectory, formatted as
a Cabal package so that it can be built with Stack.

Note to self: consider reorganizing this like the standard
`xmonad-testing` repo, where even the `xmonad.hs` file would be built
by Stack/Cabal into an executable.
