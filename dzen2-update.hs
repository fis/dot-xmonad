import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Array
import Data.Function
import Data.Int
import Data.Ix
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Word
import System.Exit
import System.IO
import Text.Regex.Posix

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import qualified Graphics.X11.Xinerama as XI

import qualified DBus as DB
import qualified DBus.Client as DBC

import Zem.DzenTwo
import Zem.StatusUpdate

-- configuration

myDzen2 = "dzen2"

myColors = Map.fromList
             [ ("default", ("#808080", "#202020"))
             , ("ws-own", ("#d0d0d0", "#606060"))
             , ("ws-other", ("#a0a0a0", "#303030"))
             , ("ws-hidden", ("#909090", "#202020"))
             , ("ws-empty", ("#606060", "#202020"))
             , ("ws-urgent", ("#ffffff", "#700000"))
             , ("title", ("#d0d0d0", "#202020"))
             ]

myFont = "DejaVu Sans:size=10"
myBarHeight = 20

myScreens :: Maybe [((Int,Int),(Int,Int))]
myScreens = Nothing -- will use Xinerama to find out

-- trivial helpers to access the configuration

color :: String -> (String, String)
color c = Map.findWithDefault ("#ffffff", "#ff0000") c myColors

-- main application

data Event = XMonadUpdate StatusUpdate
           | DzenActivity Int String
           | QuitEvent
           deriving Show

data BarState = BarState {
  barScreenCount :: Int,
  barActiveScreen :: Int,
  barWorkspaces :: [[WS]],
  barLayouts :: [String],
  barTitles :: [String]
  }
  deriving Show

type BarIO = StateT BarState IO

main :: IO ()
main = do
  -- connect to X display for sending events to xmonad
  dpy <- X.openDisplay ""
  -- extract list of connected screens
  screens <- maybe (xineramaQuery dpy) return myScreens
  -- set up a channel for event-receiving
  eventChan <- newChan
  -- set up DBus listener to feed the event channel
  dbusSetupListener eventChan
  -- start a dzen2 process for each screen, fork a thread to get events
  dzenHandles <- mapM startBar screens
  mapM_ (readBar eventChan) $ zip [0..] dzenHandles
  -- handle events until the end
  evalStateT (forever $ handleEvents eventChan dzenHandles dpy) $ initState screens
  where
    startBar :: ((Int,Int),(Int,Int)) -> IO (Handle, Handle)
    startBar ((x, y), (w, _)) =
      startDzen2 myDzen2 ((x, y), (w, myBarHeight)) (color "default") myFont
    readBar :: Chan Event -> (Int, (Handle, Handle)) -> IO ()
    readBar eventChan (n, (_, h)) = readDzen2 h $ writeChan eventChan . DzenActivity n
    initState :: [((Int,Int),(Int,Int))] -> BarState
    initState screens = BarState {
      barScreenCount = length screens,
      barActiveScreen = 0,
      barWorkspaces = [] <$ screens,
      barLayouts = "Tall" <$ screens,
      barTitles = "(dzen2)" <$ screens
      }

handleEvents :: Chan Event -> [(Handle, Handle)] -> X.Display -> BarIO ()
handleEvents chan dzen dpy = do
  event <- liftIO $ readChan chan
  handle event
  where
    handle :: Event -> BarIO ()
    -- status updates from xmonad
    handle (XMonadUpdate msg) = do
      -- update current state
      oldState <- get
      let (newState, toUpdate) = parseUpdate msg oldState
      put newState
      -- redraw necessary dzen toolbars
      mapM_ update toUpdate
    -- click events from dzen
    handle (DzenActivity screen event) =
      liftIO $ maybe
        (return ())
        (handleDzenClick screen)
        (matchOnceText (makeRegex "^m1ws([0-9]+)" :: Regex) event)
    -- quit events for a restarting XMonad
    handle QuitEvent = liftIO exitSuccess
    -- workspace-switching click events
    handleDzenClick :: Int -> (String, MatchText String, String) -> IO ()
    handleDzenClick scr (_, match, _) =
      let ws = fst . (! 1) $ match in
      toXMonad "XMONAD_SWITCH" (scr * 65536 + read ws)
    -- function to update some statusbars
    update :: Int -> BarIO ()
    update idx = do
      state <- get
      liftIO . hPutStrLn (fst (dzen !! idx)) . show $ makeBar state idx
    -- function to send stuff to running xmonad
    toXMonad :: String -> Int -> IO ()
    toXMonad mtype arg = do
      rw <- X.rootWindow dpy $ X.defaultScreen dpy
      a <- X.internAtom dpy mtype False
      X.allocaXEvent $ \e -> do
        XE.setEventType e X.clientMessage
        XE.setClientMessageEvent e rw a 32 (fromIntegral arg) XE.currentTime
        X.sendEvent dpy rw False X.structureNotifyMask e
        X.sync dpy False

parseUpdate :: StatusUpdate -> BarState -> (BarState, [Int])
parseUpdate (StatusUpdate screen workspaces layout title) oldState =
  let screenList = [0..((barScreenCount oldState) - 1)]
      newActiveScreen = fromEnum screen :: Int
      newWorkspaces = map getWorkspaces screenList
      newState = oldState
                   { barActiveScreen = newActiveScreen
                   , barWorkspaces = newWorkspaces
                   , barLayouts = insertTo newActiveScreen layout $ barLayouts oldState
                   , barTitles = insertTo newActiveScreen title $ barTitles oldState
                   } in
  (newState,
   findIndices (not . and) . transpose $
     [ zipWith (==) newWorkspaces (barWorkspaces oldState)
     , zipWith (==) (barLayouts newState) (barLayouts oldState)
     , zipWith (==) (barTitles newState) (barTitles oldState)
     ])
  where
    getWorkspaces :: Int -> [WS]
    getWorkspaces scr = map (parseWorkspace scr) workspaces
    parseWorkspace :: Int -> (String, WSType, Int, Bool) -> WS
    parseWorkspace scr (tag, kind, wscr, urg) = WS tag kind (scr == wscr) urg
    insertTo :: Int -> a -> [a] -> [a]
    insertTo at title old =
      let (before, (_:after)) = splitAt at old in
      before ++ (title:after)

-- status line formatting code

makeBar :: BarState -> Int -> D2Text
makeBar state idx = D2T $ workspaces ++ [sep, layout, sep, title]
  where
    workspaces :: [D2Text]
    workspaces = intersperse (D2Raw " ") . map makeWS . zip [0..] $ barWorkspaces state !! idx
    layout :: D2Text
    layout = dzen2LayoutIcon $ barLayouts state !! idx
    title :: D2Text
    title = D2Color (color "title") . D2Lit $ barTitles state !! idx
    sep :: D2Text
    sep = D2Sep 4 myBarHeight
    makeWS :: (Int, WS) -> D2Text
    makeWS (idx, WS name wtype ss urg) =
      D2Clickable ("ws" ++ show idx) . makeName wtype ss urg . D2Gap 2 $ D2T [makeIcon wtype, D2Lit name]
    makeIcon :: WSType -> D2Text
    makeIcon WSEmpty = D2Raw "^ro(6x6)^r(2x0)"
    makeIcon _       = D2Raw "^r(6x6)^r(2x0)"
    makeName :: WSType -> Bool -> Bool -> D2Text -> D2Text
    makeName _         _     True = D2Color (color "ws-urgent")
    makeName WSCurrent True  _    = D2Color (color "ws-own")
    makeName WSCurrent False _    = D2Color (color "ws-other")
    makeName WSVisible True  _    = D2Color (color "ws-own")
    makeName WSVisible False _    = D2Color (color "ws-other")
    makeName WSHidden  _     _    = D2Color (color "ws-hidden")
    makeName WSEmpty   _     _    = D2Color (color "ws-empty")

dzen2LayoutIcon :: String -> D2Text
dzen2LayoutIcon "Tall" = D2Raw "◧"
dzen2LayoutIcon "Wide" = D2Raw "⬒"
dzen2LayoutIcon "Full" = D2Raw "^ro(12x12)"
dzen2LayoutIcon str = D2Lit str

-- dbus event listening code

dbusSetupListener :: Chan Event -> IO ()
dbusSetupListener eventChan = listenStatus handle
  where
    handle :: String -> [DB.Variant] -> IO ()
    handle "StatusUpdate" [scr, ws, layout, title] = case unpackUpdate scr ws layout title of
      Just update -> writeChan eventChan . XMonadUpdate $ update
      Nothing -> return ()
    handle "Shutdown" _ = writeChan eventChan QuitEvent

-- xinerama screen query

xineramaQuery :: X.Display -> IO [((Int,Int),(Int,Int))]
xineramaQuery dpy =
  map getCoords <$> XI.getScreenInfo dpy
  where
    getCoords :: X.Rectangle -> ((Int,Int),(Int,Int))
    getCoords r = ((fromIntegral $ X.rect_x r, fromIntegral $ X.rect_y r),
                   (fromIntegral $ X.rect_width r, fromIntegral $ X.rect_height r))
