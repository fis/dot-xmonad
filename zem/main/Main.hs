import XMonad
import Zem.Config (zemConfig)

main = zemConfig >>= xmonad
