import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import qualified XMonad.Layout.HintedTile as HT
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutHints
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

myLayouts = desktopLayoutModifiers $ hintedTile HT.Tall ||| hintedTile HT.Wide ||| Full
  where
    hintedTile = HT.HintedTile nmaster delta ratio HT.TopLeft
    nmaster    = 1
    ratio      = 1/2
    delta      = 3/100

main = do
  -- open the DBus connection for status updates
  dbus <- DBC.connectSession
  -- start XMonad
  xmonad $ withUrgencyHook NoUrgencyHook $ gnomeConfig {
    workspaces = withScreens (S $ length myScreens) myWorkspaces,
    modMask = myModm,
    terminal = myTerminal,
    logHook = myDBusLogHook dbus >> logHook gnomeConfig,
    layoutHook = myLayouts
    }
    `additionalKeys` myKeys

-- dbus status update code

myDBusLogHook :: DBC.Client -> X ()
myDBusLogHook c = do
  status <- dynamicLogString dbusPP
  io $ DBC.emit c path ifc mem [DBT.toVariant status]
  where
    path = DBT.objectPath_ $ T.pack "/fi/zem/xmonad/status"
    ifc = DBT.interfaceName_ $ T.pack "fi.zem.XMonad.Status"
    mem = DBT.memberName_ $ T.pack "StatusUpdate"
    dbusPP = defaultPP {
      ppCurrent = (++"/c"),
      ppVisible = (++"/v"),
      ppHidden = (++"/h"),
      ppHiddenNoWindows = (++"/e"),
      ppUrgent = (++"/u"),
      ppSep = ";", ppWsSep = ",",
      ppTitle = id, ppLayout = id
      }
