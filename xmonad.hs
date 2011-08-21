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
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare

import Control.Monad (liftM,when)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import System.IO
import System.Process

import qualified DBus.Address as DBA
import qualified DBus.Client.Simple as DBC
import qualified DBus.Types as DBT

myModm = mod4Mask
myTerminal = "urxvt"

myWorkspaces = ["web", "com", "work1", "work2", "x1", "x2", "x3", "x4"]

myKeys conf = [
  ((myModm, xK_Return), spawn myTerminal),
  ((myModm, xK_a), namedScratchpadAction myScratchpads "scratchterm"),
  ((myModm .|. shiftMask, xK_Return), windows W.swapMaster),
  ((myModm, xK_r), gnomeRun)
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

myScratchpads = [
  NS "scratchterm" (myTerminal ++ " -name scratchterm") (resource =? "scratchterm") centeredFloating
  ]
  where
    centeredFloating = customFloating $ W.RationalRect 0.25 0.25 0.5 0.5

myManageHook = composeAll [
  className =? "Putty" --> doFloat,
  namedScratchpadManageHook myScratchpads
  ]

main = do
  -- open the DBus connection for status updates
  dbus <- DBC.connectSession
  -- start XMonad
  nScreens <- countScreens
  let conf = withUrgencyHook NoUrgencyHook $ gnomeConfig {
        workspaces = withScreens nScreens myWorkspaces,
        modMask = myModm,
        terminal = myTerminal,
        layoutHook = myLayouts,
        manageHook = myManageHook <+> manageHook gnomeConfig,
        logHook = myDBusLogHook dbus >> takeTopFocus >> logHook gnomeConfig,
        handleEventHook = mappend myClientMessageEventHook $ handleEventHook gnomeConfig
        }
  xmonad $ conf `additionalKeys` myKeys conf

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
      ppSort = liftM (namedScratchpadFilterOutWorkspace .) getSortByIndex,
      ppTitle = id, ppLayout = id
      }

-- XClientMessageEvent listener for receiving commands back

myClientMessageEventHook :: Event -> X All
myClientMessageEventHook (ClientMessageEvent {ev_message_type = mt, ev_data = dt}) = do
  d <- asks display
  a <- io $ internAtom d "XMONAD_SWITCH" False
  when (mt == a && dt /= []) $ do
    let arg = (fromIntegral (head dt) :: Int)
        scr = arg `div` 65536
        ws = arg `mod` 65536
    windows . W.greedyView . marshall (S scr) $ myWorkspaces !! ws
  return $ All True
myClientMessageEventHook _ = return $ All True

-- code lifted from darcs xmonad-contrib XMonad.Hooks.ICCCMFocus

atom_WM_TAKE_FOCUS :: X Atom
atom_WM_TAKE_FOCUS = getAtom "WM_TAKE_FOCUS"

takeFocusX :: Window -> X ()
takeFocusX w =
  withWindowSet . const $ do
    dpy       <- asks display
    wmtakef   <- atom_WM_TAKE_FOCUS
    wmprot    <- atom_WM_PROTOCOLS
    protocols <- io $ getWMProtocols dpy w
    when (wmtakef `elem` protocols) $
      io . allocaXEvent $ \ev -> do
        setEventType ev clientMessage
        setClientMessageEvent ev w wmprot 32 wmtakef currentTime
        sendEvent dpy w False noEventMask ev

takeTopFocus :: X ()
takeTopFocus =
  (withWindowSet $ maybe (setFocusX =<< asks theRoot) takeFocusX . W.peek) -- >> setWMName "LG3D"
