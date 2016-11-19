import XMonad
import XMonad.Actions.NoBorders
import XMonad.Actions.OnScreen
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import qualified XMonad.Layout.HintedTile as HT
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen (ifWider)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run (safeSpawn, safeSpawnProg, unsafeSpawn)
import XMonad.Util.WorkspaceCompare

import Control.Monad (filterM, when)
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Environment (getEnv)

import qualified DBus.Client as DBC

import Zem.StatusUpdate
import Zem.Utils
import Zem.VolumeControl

myModm = mod4Mask
myTerminal = "urxvt"

myWorkspaces = ["web", "mail", "irc", "c1", "c2", "x1", "x2", "x3", "x4"]

myKeys conf dbus home =
  [ ((myModm, xK_Return), safeSpawnProg myTerminal)
  , ((myModm .|. shiftMask, xK_Return), windows W.swapMaster)
  , ((myModm, xK_a), namedScratchpadAction myScratchpads "scratchterm")
  , ((myModm, xK_s), namedScratchpadAction myScratchpads "scratchemacs")
  , ((myModm, xK_r), myRun home)
  , ((0, xK_Print), unsafeSpawn ("sleep 0.1; scrot -z -s " ++ home ++ "/img/scrot/scrot-%Y%m%d-%H%M%S.png"))
  , ((myModm, xK_Print), unsafeSpawn ("scrot -z " ++ home ++ "/img/scrot/scrot-%Y%m%d-%H%M%S.png"))
  , ((0, xK_Pause), safeSpawn "xscreensaver-command" ["-lock"])
  , ((myModm, xK_q), (io $ postStatus dbus "Shutdown" []) >> unsafeSpawn "xmonad --recompile && xmonad --restart")
  , ((myModm, xK_b), withFocused toggleBorder)
  , ((0, xF86XK_AudioLowerVolume), adjustVolumeAndNotify dbus (-2))
  , ((0, xF86XK_AudioRaiseVolume), adjustVolumeAndNotify dbus 2)
  , ((0, xF86XK_AudioMute), toggleMuteAndNotify dbus)
  ]
  ++
  [ ((m .|. myModm, k), windows $ f i)
  | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]
  ++
  [ ((m .|. myModm, k), f sc)
  | (sc, k) <- zip [0..] [xK_w, xK_e]
  , (f, m) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
  ]

myRun :: String -> X ()
myRun home = safeSpawn (home ++ "/.xmonad/dmenu_run.bash") []

-- smartBorders has issues with window growth, trying life without it:
-- old: myLayouts = (smartBorders . desktopLayoutModifiers $ hintedTile HT.Tall ||| hintedTile HT.Wide ||| Full) ||| noBorders Full
-- desktopLayoutModifiers == avoidStruts, and that has Chromium issues:
--  https://bugs.chromium.org/p/chromium/issues/detail?id=510079
--  https://github.com/xmonad/xmonad-contrib/issues/73
-- trying with manual gaps instead
-- old: myLayouts = (desktopLayoutModifiers $ hintedTile HT.Tall ||| hintedTile HT.Wide ||| Full) ||| noBorders Full
myLayouts = ifWider 1600 (gaps [(U, 20), (D, 30)] defaults) (gaps [(U, 20)] defaults) ||| noBorders Full
  where
    defaults   = hintedTile HT.Tall ||| hintedTile HT.Wide ||| Full
    hintedTile = HT.HintedTile nmaster delta ratio HT.TopLeft
    nmaster    = 1
    ratio      = 1/2
    delta      = 3/100

myScratchpads =
  [ NS "scratchterm" (myTerminal ++ " -name scratchterm -e screen -S scratchterm -dR") (resource =? "scratchterm") centeredFloating
  , NS "scratchemacs" "emacsclient -a '' -e '(scratch-frame)'" (resource =? "scratch-emacs") centeredFloating
  ]
  where
    centeredFloating = customFloating $ W.RationalRect 0.15 0.15 0.7 0.7

myManageHook =
  composeAll [ manageAndroidStudioPopups
             , isFullscreen --> doFullFloat
             , className =? "Putty" --> doFloat
             , className =? "net-minecraft-MinecraftLauncher" --> doFloat
             , className =? "Xfce4-notifyd" --> doFloat
             , className =? "Wine" --> doFloat
             , namedScratchpadManageHook myScratchpads
             ]

-- main function

main = do
  home <- getEnv "HOME"
  -- open the DBus connection for status updates
  dbus <- DBC.connectSession
  -- start dzen2-update if it's not running yet
  safeSpawn (home ++ "/.xmonad/dzen2-update") []
  -- start XMonad
  let conf = desktopConfig
               { workspaces = myWorkspaces
               , modMask = myModm
               , terminal = myTerminal
               , layoutHook = myLayouts
               , manageHook = myManageHook <+> manageHook desktopConfig
               , logHook = myDBusLogHook dbus >> logHook desktopConfig
               , handleEventHook = myClientMessageEventHook <+> fullscreenEventHook <+> handleEventHook desktopConfig
               -- This should in theory be no longer necessary: (TODO: cleanup)
               , startupHook = setWMName "LG3D"
               }
  xmonad $ (withUrgencyHook NoUrgencyHook conf) `additionalKeys` myKeys conf dbus home

-- dbus status update code

myDBusLogHook :: DBC.Client -> X ()
myDBusLogHook client = withWindowSet log >>= io . postStatusUpdate client
  where
    log :: WindowSet -> X StatusUpdate
    log ws = do
      workspaces <- getWorkspaces
      title <- getTitle
      return $ StatusUpdate screen workspaces layout title
      where
        -- current screen
        screen = screenId . W.screen . W.current $ ws
        -- workspace list with details
        getWorkspaces = do
          idx <- getWsIndex
          urgs <- readUrgents
          return . wsUrg urgs . wsSort idx $ wsCurrent : (wsVisible ++ wsHidden)
        wsCurrent = wsVisInfo WSCurrent . W.current $ ws
        wsVisible = map (wsVisInfo WSVisible) . W.visible $ ws
        wsHidden = map wsHidInfo . namedScratchpadFilterOutWorkspace . W.hidden $ ws
        wsVisInfo kind w = (W.workspace w, kind, screenId . W.screen $ w)
        wsHidInfo w = (w, if isNothing . W.stack $ w then WSEmpty else WSHidden, -1)
        wsSort idx = sortBy (compare `on` idx . wsTag)
        wsTag (a, _, _) = W.tag a
        wsUrg urgs = map (\(w, k, s) -> (W.tag w, k, s, any (\x -> maybe False (== W.tag w) (W.findTag x ws)) urgs))
        -- layout description
        layout = description . W.layout . W.workspace . W.current $ ws
        -- current title
        getTitle = maybe (return "") (fmap show . getName) . W.peek $ ws
        -- helpers
        screenId :: ScreenId -> Int
        screenId (S s) = toEnum s

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
  a <- io $ internAtom d "XMONAD_NSP_SHOW" False
  when (mt == a && dt /= []) $ do
    let (NS name _ query _) = myScratchpads !! (fromIntegral (head dt) :: Int)
    withWindowSet $ \s -> do
      current <- filterM (runQuery query) ((maybe [] W.integrate . W.stack . W.workspace . W.current) s)
      when (null current) $ namedScratchpadAction myScratchpads name
  return $ All True
myClientMessageEventHook _ = return $ All True
