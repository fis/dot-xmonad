import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

import Data.Maybe
import qualified Data.Text as T
import System.IO
import System.Process

import qualified DBus.Address as DBA
import qualified DBus.Client.Simple as DBC
import qualified DBus.Types as DBT

myModm = mod4Mask
myTerminal = "urxvt"
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
  -- open the DBus connection for status updates
  dbus <- DBC.connectSession
  -- start XMonad
  xmonad $ gnomeConfig {
    workspaces = withScreens (S $ length myScreens) myWorkspaces,
    modMask = myModm,
    terminal = myTerminal,
    logHook = myDBusLogHook dbus >> logHook gnomeConfig
    }
    `additionalKeys` myKeys

-- dbus status update code

myDBusLogHook :: DBC.Client -> X ()
myDBusLogHook c = do
  status <- dynamicLogString defaultPP
  io $ DBC.emit c path ifc mem [DBT.toVariant status]
  where
    path = DBT.objectPath_ $ T.pack "/fi/zem/xmonad/status"
    ifc = DBT.interfaceName_ $ T.pack "fi.zem.XMonad.Status"
    mem = DBT.memberName_ $ T.pack "StatusUpdate"
