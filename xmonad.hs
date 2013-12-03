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
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare

import Control.Monad (ap,liftM,when)
import qualified Data.ByteString.Char8 as B
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import System.IO
import System.Process

import qualified DBus as DB
import qualified DBus.Client as DBC

myModm = mod4Mask
myTerminal = "urxvt"

myWorkspaces = ["web", "mail", "irc", "c1", "c2", "x1", "x2", "x3", "x4"]

myKeys conf dbus =
  [ ((myModm, xK_Return), spawn myTerminal)
  , ((myModm .|. shiftMask, xK_Return), windows W.swapMaster)
  , ((myModm, xK_a), namedScratchpadAction myScratchpads "scratchterm")
  , ((myModm, xK_r), gnomeRun)
  , ((0, xK_Print), spawn "gnome-screenshot -i")
  , ((myModm, xK_q), dbusPost dbus "Shutdown" "" >> spawn "xmonad --recompile && xmonad --restart")
  ]
  ++
  [ ((m .|. myModm, k), windows $ f i)
  | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

myLayouts = smartBorders $ (desktopLayoutModifiers $ hintedTile HT.Tall ||| hintedTile HT.Wide ||| Full) ||| Full
  where
    hintedTile = HT.HintedTile nmaster delta ratio HT.TopLeft
    nmaster    = 1
    ratio      = 1/2
    delta      = 3/100

myScratchpads =
  [ NS "scratchterm" (myTerminal ++ " -name scratchterm") (resource =? "scratchterm") centeredFloating
  ]
  where
    centeredFloating = customFloating $ W.RationalRect 0.25 0.25 0.5 0.5

myManageHook =
  composeAll [ isFullscreen --> doFullFloat
             , className =? "Putty" --> doFloat
             , namedScratchpadManageHook myScratchpads
             ]

main = do
  -- open the DBus connection for status updates
  dbus <- DBC.connectSession
  -- start dzen2-update if it's not running yet
  spawn "./.xmonad/dzen2-update"
  -- start XMonad
  let conf = gnomeConfig
               { workspaces = myWorkspaces
               , modMask = myModm
               , terminal = myTerminal
               , layoutHook = myLayouts
               , manageHook = myManageHook <+> manageHook gnomeConfig
               , logHook = myDBusLogHook dbus >> takeTopFocus >> logHook gnomeConfig
               , handleEventHook = myClientMessageEventHook <+> fullscreenEventHook <+> handleEventHook gnomeConfig
               }
  xmonad $ (withUrgencyHook NoUrgencyHook conf) `additionalKeys` myKeys conf dbus

-- dbus status update code

myDBusLogHook :: DBC.Client -> X ()
myDBusLogHook client = withWindowSet log >>= dbusPostRaw client "StatusUpdate"
  where
    log :: WindowSet -> X [DB.Variant]
    log ws = do
      workspaces <- getWorkspaces
      title <- getTitle
      return [DB.toVariant (screen, workspaces, layout, title)]
      where
        -- current screen
        screen = screenId . W.screen . W.current $ ws
        -- workspace list with details
        getWorkspaces = do
          idx <- getWsIndex
          urgs <- readUrgents
          return . wsUrg urgs . wsSort idx $ wsCurrent : (wsVisible ++ wsHidden)
        wsCurrent = wsVisInfo (0 :: Int32) . W.current $ ws
        wsVisible = map (wsVisInfo (1 :: Int32)) . W.visible $ ws
        wsHidden = map wsHidInfo . namedScratchpadFilterOutWorkspace . W.hidden $ ws
        wsVisInfo kind w = (W.workspace w, kind, screenId . W.screen $ w)
        wsHidInfo w = (w, if isNothing . W.stack $ w then 3 else 2 :: Int32, -1 :: Int32)
        wsSort idx = sortBy (compare `on` idx . wsTag)
        wsTag (a, _, _) = W.tag a
        wsUrg urgs = map (\(w, k, s) -> (W.tag w, k, s, any (\x -> maybe False (== W.tag w) (W.findTag x ws)) urgs))
        -- layout description
        layout = description . W.layout . W.workspace . W.current $ ws
        -- current title
        getTitle = fmap B.pack . maybe (return "") (fmap show . getName) . W.peek $ ws
        -- helpers
        screenId :: ScreenId -> Int32
        screenId (S s) = toEnum s

dbusPost :: DBC.Client -> String -> String -> X ()
dbusPost c m s = dbusPostRaw c m [DB.toVariant . B.pack $ s]

dbusPostRaw :: DBC.Client -> String -> [DB.Variant] -> X ()
dbusPostRaw client member body = io $ DBC.emit client sig
  where
    sig = (DB.signal path iface memName) { DB.signalBody = body }
    path = DB.objectPath_ "/fi/zem/xmonad/status"
    iface = DB.interfaceName_ "fi.zem.XMonad.Status"
    memName = DB.memberName_ member

-- XClientMessageEvent listener for receiving commands back

myClientMessageEventHook :: Event -> X All
myClientMessageEventHook (ClientMessageEvent {ev_message_type = mt, ev_data = dt}) = do
  d <- asks display
  a <- io $ internAtom d "XMONAD_SWITCH" False
  when (mt == a && dt /= []) $ do
    let arg = (fromIntegral (head dt) :: Int)
        scr = arg `div` 65536
        ws = arg `mod` 65536
    windows . greedyViewOnScreen (S scr) $ myWorkspaces !! ws
  return $ All True
myClientMessageEventHook _ = return $ All True
