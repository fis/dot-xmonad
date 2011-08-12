import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myModm = mod4Mask
myTerminal = "urxvt"

myWorkspaces = ["web", "com", "work1", "work2", "x1", "x2", "x3", "x4"]

myKeys =
    [
    ((myModm, xK_Return), spawn myTerminal),
    ((myModm .|. shiftMask, xK_Return), windows W.swapMaster),
    ((myModm, xK_r), gnomeRun)
    ]
    ++
    [
    ((m .|. myModm, k), windows $ onCurrentScreen f i) |
      (i, k) <- zip myWorkspaces [xK_1 .. xK_9],
      (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

main = do
    dzenproc <- spawnPipe "/home/htkallas/local/bin/dzen2 -ta l"
    xmonad $ gnomeConfig {
        workspaces = withScreens 2 myWorkspaces,
        modMask = myModm,
        terminal = myTerminal,
        logHook = do
            dynamicLogWithPP dzenPP {
                ppOutput = hPutStrLn dzenproc
                }
            logHook gnomeConfig
        -- manageHook = manageDocks <+> manageHook gnomeConfig,
        -- layoutHook = avoidStruts $ layoutHook gnomeConfig
        }
        `additionalKeys` myKeys
