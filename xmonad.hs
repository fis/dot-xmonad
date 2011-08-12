import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

import Control.Applicative
import System.IO
import System.Process

myModm = mod4Mask
myTerminal = "urxvtc"
myDzen = "/home/htkallas/local/bin/dzen2"

myScreens = [(1280,1024), (1600,1200)]
myWorkspaces = ["web", "com", "work1", "work2", "x1", "x2", "x3", "x4"]

myKeys =
  [ ((myModm, xK_Return), spawn myTerminal)
  , ((myModm .|. shiftMask, xK_Return), windows W.swapMaster)
  , ((myModm, xK_r), gnomeRun)
  ]
  ++
  [ ((m .|. myModm, k), windows $ onCurrentScreen f i)
  | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

main = do
  (toDzen0, fromDzen0) <- myRunDzen 0
  (toDzen1, fromDzen1) <- myRunDzen 1
  xmonad $ gnomeConfig {
    workspaces = withScreens (S $ length myScreens) myWorkspaces,
    modMask = myModm,
    terminal = myTerminal,
    logHook = do
      dynamicLogWithPP dzenPP {
        ppOutput = \s -> sequence_ $ [hPutStrLn toDzen0, hPutStrLn toDzen1] <*> [s]
        }
        logHook gnomeConfig
    }
    `additionalKeys` myKeys

-- dzen spawning for different screens

myRunDzen :: Int -> IO (Handle, Handle)
myRunDzen s = do
  (inp, out, err, pid) <- runInteractiveProcess myDzen args Nothing Nothing
  hSetBuffering inp LineBuffering
  hSetBuffering out LineBuffering
  return (inp, out)
  where
    args :: [String]
    args = ["-ta", "l", "-p", "-x", xpos, "-y", "0", "-h", "16", "-w", width]
    xpos :: String
    xpos = show . sum $ fst `map` take s myScreens
    width :: String
    width = show . fst $ myScreens !! s
