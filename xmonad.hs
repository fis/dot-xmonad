import XMonad
import XMonad.Actions.OnScreen
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import qualified XMonad.Layout.HintedTile as HT
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare

import Control.Monad (liftM,when)
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import System.IO
import System.Process

import qualified DBus as DB
import qualified DBus.Client as DBC

myModm = mod4Mask
myTerminal = "urxvt"

myWorkspaces = ["web", "com", "work1", "work2", "x1", "x2", "x3", "x4"]

myKeys conf dbus = [
  ((myModm, xK_Return), spawn myTerminal),
  ((myModm .|. shiftMask, xK_Return), windows W.swapMaster),
  ((myModm, xK_a), namedScratchpadAction myScratchpads "scratchterm"),
  ((myModm, xK_r), gnomeRun),
  ((0, xK_Print), spawn "gnome-screenshot -i"),
  ((myModm, xK_q), dbusPost dbus "Shutdown" "" >> spawn "xmonad --recompile && xmonad --restart")
  ]
  ++
  [ ((m .|. myModm, k), windows $ onCurrentScreen f i)
  | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

myLayouts = smartBorders $ (desktopLayoutModifiers $ hintedTile HT.Tall ||| hintedTile HT.Wide ||| Full) ||| Full
  where
    hintedTile = HT.HintedTile nmaster delta ratio HT.TopLeft
    nmaster    = 1
    ratio      = 1/2
    delta      = 3/100

myScratchpads = [
  NS "scratchterm" (myTerminal ++ " -name scratchterm") (resource =? "scratchterm") centeredFloating
  ]
  where
    centeredFloating = customFloating $ W.RationalRect 0.25 0.25 0.5 0.5

myManageHook = composeAll [
  isFullscreen --> doFullFloat,
  className =? "Putty" --> doFloat,
  namedScratchpadManageHook myScratchpads
  ]

main = do
  -- open the DBus connection for status updates
  dbus <- DBC.connectSession
  -- start dzen2-update if it's not running yet
  spawn "./.xmonad/dzen2-update"
  -- start XMonad
  nScreens <- countScreens
  let conf = withUrgencyHook NoUrgencyHook $ gnomeConfig {
        workspaces = withScreens nScreens myWorkspaces,
        modMask = myModm,
        terminal = myTerminal,
        layoutHook = myLayouts,
        manageHook = myManageHook <+> manageHook gnomeConfig,
        logHook = myDBusLogHook dbus >> takeTopFocus >> logHook gnomeConfig,
        handleEventHook = myClientMessageEventHook <+> fullscreenEventHook <+> handleEventHook gnomeConfig
        }
  xmonad $ conf `additionalKeys` myKeys conf dbus

-- dbus status update code

myDBusLogHook :: DBC.Client -> X ()
myDBusLogHook c = dynamicLogString dbusPP >>= dbusPost c "StatusUpdate"
  where
    dbusPP = defaultPP {
      ppCurrent = (++"/c"),
      ppVisible = (++"/v"),
      ppHidden = (++"/h"),
      ppHiddenNoWindows = (++"/e"),
      ppUrgent = (++"/u"),
      ppSep = ";", ppWsSep = ",",
      ppSort = liftM (namedScratchpadFilterOutWorkspace .) getSortByIndex,
      ppTitle = id,
      ppLayout = id
      }

dbusPost :: DBC.Client -> String -> String -> X ()
dbusPost c m s = io $ DBC.emit c sig
  where
    sig = (DB.signal path ifc mem) { DB.signalBody = body }
    path = DB.objectPath_ "/fi/zem/xmonad/status"
    ifc = DB.interfaceName_ "fi.zem.XMonad.Status"
    mem = DB.memberName_ m
    body = [DB.toVariant . B.pack $ s]

-- XClientMessageEvent listener for receiving commands back

myClientMessageEventHook :: Event -> X All
myClientMessageEventHook (ClientMessageEvent {ev_message_type = mt, ev_data = dt}) = do
  d <- asks display
  a <- io $ internAtom d "XMONAD_SWITCH" False
  when (mt == a && dt /= []) $ do
    let arg = (fromIntegral (head dt) :: Int)
        scr = arg `div` 65536
        ws = arg `mod` 65536
    windows . greedyViewOnScreen (S scr) . marshall (S scr) $ myWorkspaces !! ws
  return $ All True
myClientMessageEventHook _ = return $ All True
