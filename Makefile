.PHONY : all

all: xmonad-x86_64-linux dzen2-update

xmonad-x86_64-linux: xmonad.hs lib/Zem/StatusUpdate.hs lib/Zem/VolumeControl.hs
	xmonad --recompile

dzen2-update: dzen2-update.hs lib/Zem/DzenTwo.hs lib/Zem/StatusUpdate.hs
	ghc -ilib --make dzen2-update
