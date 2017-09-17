{-# OPTIONS_GHC -lxklavier #-}

import XMonad
import XMonad.Actions.GridSelect (goToSelected)
import XMonad.Actions.Navigation2D
  (centerNavigation, layoutNavigation, navigation2D, singleWindowRect, windowGo, windowSwap, unmappedWindowRect)
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.OnScreen (greedyViewOnScreen)
import XMonad.Actions.PhysicalScreens (PhysicalScreen, getScreen, sendToScreen, viewScreen)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.Warp (warpToScreen)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.UrgencyHook (NoUrgencyHook(..), readUrgents, withUrgencyHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.BinarySpacePartition (FocusParent(..), TreeBalance(..), Rotate(..), Swap(..), emptyBSP)
import XMonad.Layout.BorderResize (borderResize)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
  (NamedScratchpad(NS), customFloating, namedScratchpadAction, namedScratchpadFilterOutWorkspace, namedScratchpadManageHook)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn, safeSpawnProg, unsafeSpawn)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.WorkspaceCompare (getWsIndex)

import qualified XMonad.StackSet as W

import Control.Monad (filterM, when)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (isNothing)
import Data.Monoid (All(..))
import Data.Ratio ((%))
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioRaiseVolume)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess), exitWith)

import qualified Data.Map as M
import qualified DBus.Client as DBC

import Zem.StatusUpdate (StatusUpdate(..), WS(..), WSType(..), postStatus, postStatusUpdate)
import Zem.Utils (manageAndroidStudioPopups, addNetSupported)
import Zem.VolumeControl (adjustVolumeAndNotify, toggleMuteAndNotify)
import Zem.XkbSwitch (switchKeyboardLayout)

workspaceNames = ["web", "mail", "irc", "c1", "c2", "x1", "x2", "x3", "x4"]
terminalCommand = "urxvt"

layouts = (borderResize . smartBorders . avoidStruts $ emptyBSP) ||| noBorders Full

-- key bindings

customKeys conf dbus home = let modM = modMask conf in
  -- starting and recalling things
  [ ((modM, xK_Return), safeSpawnProg terminalCommand)
  , ((modM, xK_r), safeSpawn (home ++ "/.xmonad/dmenu_run.bash") [])
  , ((modM, xK_a), namedScratchpadAction scratchpadList "scratchterm")
  , ((modM, xK_s), namedScratchpadAction scratchpadList "scratchemacs")
  , ((modM, xK_g), goToSelected def)
  -- layout management
  , ((modM, xK_f), sendMessage Rotate)
  , ((modM .|. shiftMask, xK_f), sendMessage Swap)
  , ((modM, xK_q), sendMessage Equalize)
  , ((modM .|. shiftMask, xK_q), sendMessage Balance)
  , ((modM, xK_p), sendMessage FocusParent)
  , ((modM, xK_b), withFocused toggleBorder)
  -- session management
  , ((modM, xK_x), (io $ postStatus dbus "Shutdown" []) >> unsafeSpawn ("make -C " ++ home ++ "/.xmonad && xmonad --restart"))
  , ((modM .|. shiftMask, xK_x), io (exitWith ExitSuccess))
  -- keyboard layout control
  , ((modM, xK_l), submap . M.fromList $
      [ ((0, xK_1), switchKeyboardLayout 0)
      , ((0, xK_2), switchKeyboardLayout 1)
      ])
  -- special keys (printscreen, lock, media)
  , ((0, xK_Print), unGrab >> safeSpawn "scrot" ["-z", "-s", home ++ "/img/scrot/scrot-%Y%m%d-%H%M%S.png"])
  , ((modM, xK_Print), unGrab >> safeSpawn "scrot" ["-z", home ++ "/img/scrot/scrot-%Y%m%d-%H%M%S.png"])
  , ((0, xK_Pause), safeSpawn "xscreensaver-command" ["-lock"])
  , ((0, xF86XK_AudioLowerVolume), adjustVolumeAndNotify dbus (-2))
  , ((0, xF86XK_AudioRaiseVolume), adjustVolumeAndNotify dbus 2)
  , ((0, xF86XK_AudioMute), toggleMuteAndNotify dbus)
  ]
  ++
  -- mod-1..9: workspace switching
  [ ((modM .|. m, k), windows $ f i)
  | (i, k) <- zip workspaceNames [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]
  ++
  -- mod-w..e: screen switching
  [ ((modM .|. m, k), f sc)
  | (sc, k) <- zip [0..] [xK_w, xK_e]
  , (f, m) <- [(viewOrWarp, 0), (sendToScreen, shiftMask)]
  ]

viewOrWarp :: PhysicalScreen -> X ()
viewOrWarp p = do
  msc <- getScreen p
  whenJust msc $ \sc -> do
    ws <- gets windowset
    if W.screen (W.current ws) /= sc
      then viewScreen p
      else warpToScreen sc (1 % 2) (1 % 2)

-- Navigation2D configuration

navigation2DConfig conf = let modM = modMask conf in
  navigation2D (def { layoutNavigation   = [("Full", centerNavigation)]
                    , unmappedWindowRect = [("Full", singleWindowRect)]
                    })
               (xK_Up, xK_Left, xK_Down, xK_Right)
               [(modM, windowGo),
                (modM .|. shiftMask, windowSwap)]
               False

-- NamedScratchpad configuration

scratchpadList =
  [ NS "scratchterm" (terminalCommand ++ " -name scratchterm -e screen -S scratchterm -dR") (resource =? "scratchterm") centeredFloating
  , NS "scratchemacs" "emacsclient -a '' -e '(scratch-frame)'" (resource =? "scratch-emacs") centeredFloating
  ]
  where
    centeredFloating = customFloating $ W.RationalRect 0.15 0.15 0.7 0.7

-- custom window management

customWindowManageHook =
  composeAll [ manageAndroidStudioPopups
             , isFullscreen --> doFullFloat
             , className =? "Putty" --> doFloat
             , className =? "net-minecraft-MinecraftLauncher" --> doFloat
             , className =? "Xfce4-notifyd" --> doFloat
             , className =? "Wine" --> doFloat
             , namedScratchpadManageHook scratchpadList
             ]

-- main function

main = do
  home <- getEnv "HOME"
  -- open the DBus connection for status updates
  dbus <- DBC.connectSession
  -- start dzen2-update if it's not running yet
  safeSpawn (home ++ "/.xmonad/dzen2-update") []
  -- start XMonad
  let conf = def
               { workspaces = workspaceNames
               , terminal = terminalCommand
               , layoutHook = layouts
               , manageHook = customWindowManageHook <+> manageHook def
               , logHook = dbusLogHook dbus >> logHook def
               , handleEventHook = dzenClientMessageEventHook <+> fullscreenEventHook <+> handleEventHook def
               , startupHook = setWMName "LG3D" >> addNetSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]
               , modMask = mod4Mask
               }
  xmonad . navigation2DConfig conf . (`additionalKeys` customKeys conf dbus home) . withUrgencyHook NoUrgencyHook . docks . ewmh $ conf

-- dbus status update code

dbusLogHook :: DBC.Client -> X ()
dbusLogHook client = withWindowSet log >>= io . postStatusUpdate client
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

-- XClientMessageEvent listener for receiving commands back from the dzen2 bars

dzenClientMessageEventHook :: Event -> X All
dzenClientMessageEventHook (ClientMessageEvent {ev_message_type = mt, ev_data = dt}) = do
  d <- asks display
  a <- io $ internAtom d "XMONAD_SWITCH" False
  when (mt == a && dt /= []) $ do
    let arg = (fromIntegral (head dt) :: Int)
        scr = arg `div` 65536
        ws = arg `mod` 65536
    windows . greedyViewOnScreen (S scr) $ workspaceNames !! ws
  a <- io $ internAtom d "XMONAD_NSP_SHOW" False
  when (mt == a && dt /= []) $ do
    let (NS name _ query _) = scratchpadList !! (fromIntegral (head dt) :: Int)
    withWindowSet $ \s -> do
      current <- filterM (runQuery query) ((maybe [] W.integrate . W.stack . W.workspace . W.current) s)
      when (null current) $ namedScratchpadAction scratchpadList name
  return $ All True
dzenClientMessageEventHook _ = return $ All True
