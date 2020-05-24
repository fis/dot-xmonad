What's this?
============

This is just my personal `~/.xmonad/` directory.

How do I use it?
----------------

Well, if you are sure you *want* to...

These days I use this on Debian unstable, and build with Stack.

In theory all you need to do is to clone the `xmonad` and
`xmonad-contrib` repositories into the `xmonad-git` and
`xmonad-contrib-git` subdirectories. Then call `stack install`.

It'll put the standard `xmonad` binary in your stack `local-bin-path`,
but I hardly ever use that: instead, I use a `.xsession` file to
directly start the `xmonad-$arch-$os` binary. There's also a simple
build script to make XMonad's recompile action work as expected.

The custom code resides all in the `zem` subdirectory, formatted as a
Cabal package so that it can be built with Stack. Most of the bulk is
in the library (`zem/src/`), but there are also two executables to act
as the configured XMonad binaries for my desktop and laptop
setups. The only difference between them is the automatic Xrandr (and
in the future, DPI) switching, as the laptop gets used both with and
without external displays quite often.
