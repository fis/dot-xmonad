import Data.Functor ((<&>))
import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (safeSpawn)
import Zem.AutoRandr (autoRandr, autoRandrApply, output, monitor, OutputMatcher)
import Zem.Config (zemConfig)

randrConfig :: [([OutputMatcher], [String])]
randrConfig =
  [ ([int],
     ["--size", "1920x1080", "--output", "$0", "--mode", "1920x1080", "--pos", "0x0", "--primary"])
  , ([int, extL, extR],
     ["--size", "5120x1440",
      "--output", "$0", "--off",
      "--output", "$1", "--mode", "2560x1440", "--pos", "0x0",
      "--output", "$2", "--mode", "2560x1440", "--pos", "2560x0", "--primary"])
  ]
  where
    int = output "eDP-1"
    extL = monitor "DELL U2515H"
    extR = monitor "DELL U2520D"

extraKeys :: XConfig a -> [((KeyMask, KeySym), X ())]
extraKeys conf =
  [ ((modM, xK_o), autoRandrApply randrConfig [] True)
  , ((modM .|. shiftMask, xK_o),
     safeSpawn "xrandr" ["--size", "3840x2160", "--output", "eDP-1", "--mode", "3840x2160", "--pos", "0x0", "--primary"])
  ]
  where modM = modMask conf

laptopize :: XConfig a -> XConfig a
laptopize conf = (`additionalKeys` extraKeys conf) . autoRandr randrConfig [] $ conf

main :: IO ()
main = zemConfig <&> laptopize >>= xmonad
