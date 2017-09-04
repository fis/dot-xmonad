{-# OPTIONS_GHC -lxklavier #-}

import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.NoBorders
import XMonad.Actions.OnScreen
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Submap
import XMonad.Actions.Warp
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
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
import Data.Ratio
import Graphics.X11.ExtraTypes.XF86
import System.Environment (getEnv)
import System.Exit

import qualified Data.Map as M
import qualified DBus.Client as DBC

import Zem.StatusUpdate
import Zem.Utils
import Zem.VolumeControl
import Zem.XkbSwitch

myModm = mod4Mask
myTerminal = "urxvt"

myWorkspaces = ["web", "mail", "irc", "c1", "c2", "x1", "x2", "x3", "x4"]

myKeys conf dbus home =
  [ ((myModm, xK_Return), safeSpawnProg myTerminal)
  , ((myModm .|. shiftMask, xK_Return), windows W.swapMaster)
  , ((myModm, xK_q), sendMessage Equalize)
  , ((myModm .|. shiftMask, xK_q), sendMessage Balance)
  , ((myModm, xK_f), sendMessage Rotate)
  , ((myModm .|. shiftMask, xK_f), sendMessage Swap)
  , ((myModm, xK_p), sendMessage FocusParent)
  , ((myModm, xK_a), namedScratchpadAction myScratchpads "scratchterm")
  , ((myModm, xK_s), namedScratchpadAction myScratchpads "scratchemacs")
  , ((myModm, xK_r), myRun home)
  , ((0, xK_Print), unsafeSpawn ("sleep 0.1; scrot -z -s " ++ home ++ "/img/scrot/scrot-%Y%m%d-%H%M%S.png"))
  , ((myModm, xK_Print), unsafeSpawn ("scrot -z " ++ home ++ "/img/scrot/scrot-%Y%m%d-%H%M%S.png"))
  , ((0, xK_Pause), safeSpawn "xscreensaver-command" ["-lock"])
  , ((myModm, xK_x), (io $ postStatus dbus "Shutdown" []) >> unsafeSpawn ("make -C " ++ home ++ "/.xmonad && xmonad --restart"))
  , ((myModm .|. shiftMask, xK_x), io (exitWith ExitSuccess))
  , ((myModm, xK_b), withFocused toggleBorder)
  , ((myModm, xK_l), submap . M.fromList $
      [ ((0, xK_1), switchKeyboardLayout 0)
      , ((0, xK_2), switchKeyboardLayout 1)
      ])
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
  , (f, m) <- [(mySwitchScreen, 0), (sendToScreen, shiftMask)]
  ]

myRun :: String -> X ()
myRun home = safeSpawn (home ++ "/.xmonad/dmenu_run.bash") []

mySwitchScreen :: PhysicalScreen -> X ()
mySwitchScreen p = do
  msc <- getScreen p
  whenJust msc $ \sc -> do
    ws <- gets windowset
    if W.screen (W.current ws) /= sc
      then viewScreen p
      else warpToScreen sc (1 % 2) (1 % 2)

myNavigation2DConfig = def { layoutNavigation   = [("Full", centerNavigation)]
                           , unmappedWindowRect = [("Full", singleWindowRect)]
                           }
myNavigation2D =
  navigation2D myNavigation2DConfig
               (xK_Up, xK_Left, xK_Down, xK_Right)
               [(myModm, windowGo),
                (myModm .|. shiftMask, windowSwap)]
               False

myLayouts = ifWider 1600 (gaps [(U, 20), (D, 30)] defaults) (gaps [(U, 20)] defaults) ||| noBorders Full
  where
    defaults = borderResize emptyBSP

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
               , startupHook = setWMName "LG3D"
               }
  xmonad . myNavigation2D . (`additionalKeys` myKeys conf dbus home) . withUrgencyHook NoUrgencyHook $ conf

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
