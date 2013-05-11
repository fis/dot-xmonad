import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Array
import Data.Function
import Data.Ix
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Word
import System.Exit
import System.IO
import System.Process
import Text.Regex.Posix

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as XE
import qualified Graphics.X11.Xinerama as XI

import qualified DBus as DB
import qualified DBus.Client as DBC

-- configuration

myDzen2 = "/usr/bin/dzen2"

myColors = Map.fromList [
  ("default", ("#808080", "#202020")),
  ("ws-visible", ("#d0d0d0", "#606060")),
  ("ws-hidden", ("#909090", "#202020")),
  ("ws-empty", ("#606060", "#202020")),
  ("ws-urgent", ("#ffffff", "#700000")),
  ("title", ("#d0d0d0", "#202020"))
  ]

myFont = "DejaVu Sans:size=10"
myBarHeight = 20

myScreens :: Maybe [((Int,Int),(Int,Int))]
myScreens = Nothing -- will use Xinerama to find out

-- trivial helpers to access the configuration

color :: String -> (String, String)
color c = Map.findWithDefault ("#ffffff", "#ff0000") c myColors

-- main application

data Event = StatusUpdate String
           | DzenActivity Int String
           | QuitEvent
           deriving Show

data WSType = WSCurrent | WSVisible | WSHidden | WSEmpty
              deriving (Eq, Ord, Show)
data WS = WS String WSType Bool
          deriving (Eq, Show)

wsName :: WS -> String
wsName (WS name _ _) = name
wsType :: WS -> WSType
wsType (WS _ wstype _) = wstype
wsUrgent :: WS -> Bool
wsUrgent (WS _ _ urg) = urg

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
  dzenHandles <- mapM startDzen2 screens
  mapM_ (readDzen2 eventChan) $ zip [0..] dzenHandles
  -- handle events until the end
  evalStateT (forever $ handleEvents eventChan dzenHandles dpy) $ initState screens
  where
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
    handle (StatusUpdate msg) = do
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
      liftIO $ hPutStrLn (fst (dzen !! idx)) $ makeBar state idx
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

parseUpdate :: String -> BarState -> (BarState, [Int])
parseUpdate str oldState =
  let (wsstr:(layout:title)) = splitOn ";" str
      screenList = [0..((barScreenCount oldState) - 1)]
      allWorkspaces = parseWorkspaces wsstr
      newActiveScreen = fromMaybe (-1) . fmap fst . find ((== WSCurrent) . wsType . snd) $ allWorkspaces
      newWorkspaces = map (getWorkspaces allWorkspaces) screenList
      newTitle = if title == [] then "" else head title
      newState = oldState {
        barActiveScreen = newActiveScreen,
        barWorkspaces = newWorkspaces,
        barLayouts = insertTo newActiveScreen layout $ barLayouts oldState,
        barTitles = insertTo newActiveScreen newTitle $ barTitles oldState
        } in
  (newState,
   findIndices (not . and) . transpose $ [
     zipWith (==) newWorkspaces $ barWorkspaces oldState,
     zipWith (==) (barLayouts newState) (barLayouts oldState),
     zipWith (==) (barTitles newState) (barTitles oldState)
     ])
  where
    getWorkspaces :: [(Int, WS)] -> Int -> [WS]
    getWorkspaces wslist screen =
      map snd $ filter ((== screen) . fst) wslist
    parseWorkspaces :: String -> [(Int, WS)]
    parseWorkspaces = map parseWorkspace . splitOn ","
    parseWorkspace :: String -> (Int, WS)
    parseWorkspace str =
      let (_, _, _, [scr, name, wtype, urg]) = str =~ "^([0-9]+)_([^/]*)/([cvhe])(/u)?" :: (String, String, String, [String]) in
      (read scr :: Int, WS name (parseWSType wtype) (urg /= ""))
    parseWSType :: String -> WSType
    parseWSType "c" = WSCurrent
    parseWSType "v" = WSVisible
    parseWSType "h" = WSHidden
    parseWSType "e" = WSEmpty
    insertTo :: Int -> a -> [a] -> [a]
    insertTo at title old =
      let (before, (_:after)) = splitAt at old in
      before ++ (title:after)

-- status line formatting code

makeBar :: BarState -> Int -> String
makeBar state idx = workspaces ++ sep ++ layout ++ sep ++ title
  where
    workspaces :: String
    workspaces = intercalate " " $ map makeWS $ zip [0..] $ barWorkspaces state !! idx
    layout :: String
    layout = dzen2LayoutIcon $ barLayouts state !! idx
    title :: String
    title = dzen2Color (color "title") $ barTitles state !! idx
    sep :: String
    sep = "^p(+4)^r(1x" ++ show myBarHeight ++ ")^p(+4)"
    makeWS :: (Int, WS) -> String
    makeWS (idx, WS name wtype urg) =
      dzen2Clickable ("ws" ++ show idx) $ makeName wtype urg $ makeIcon wtype ++ name
    makeIcon :: WSType -> String
    makeIcon WSEmpty = "^ro(6x6)^r(2x0)"
    makeIcon _       = "^r(6x6)^r(2x0)"
    makeName :: WSType -> Bool -> String -> String
    makeName _         True = dzen2Color (color "ws-urgent") . dzen2Gap (2,2)
    makeName WSCurrent _    = dzen2Color (color "ws-visible") . dzen2Gap (2,2)
    makeName WSVisible _    = dzen2Color (color "ws-visible") . dzen2Gap (2,2)
    makeName WSHidden  _    = dzen2Color (color "ws-hidden") . dzen2Gap (2,2)
    makeName WSEmpty   _    = dzen2Color (color "ws-empty") . dzen2Gap (2,2)

dzen2Color :: (String, String) -> String -> String
dzen2Color (fg, bg) str =
  "^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")" ++ str ++ "^fg()^bg()"

dzen2Gap :: (Int, Int) -> String -> String
dzen2Gap (front, back) str =
  "^r(+" ++ show front ++ "x0)" ++ str ++ "^r(+" ++ show back ++ "x0)"

dzen2Clickable :: String -> String -> String
dzen2Clickable name str =
  "^ca(1, echo m1" ++ name ++ ")" ++ str ++ "^ca()"

dzen2LayoutIcon :: String -> String
dzen2LayoutIcon "Tall" = "◧"
dzen2LayoutIcon "Wide" = "⬒"
dzen2LayoutIcon "Full" = "^ro(12x12)"
dzen2LayoutIcon str = str

-- dzen2 process handling code

startDzen2 :: ((Int,Int),(Int,Int)) -> IO (Handle, Handle)
startDzen2 ((xpos,ypos), (width,_)) = do
  (inp, out, _, _) <- runInteractiveProcess myDzen2 args Nothing Nothing
  hSetBuffering inp LineBuffering
  hSetBuffering out LineBuffering
  hPutStrLn inp "(dzen2)"
  return (inp, out)
  where
    args :: [String]
    args =
      let (fg, bg) = color "default" in
      ["-ta", "l",
       "-x", show xpos, "-y", show ypos, "-w", show width, "-h", show myBarHeight,
       "-fg", fg, "-bg", bg,
       "-fn", myFont,
       "-e", "button3=print:m3"
      ]

readDzen2 :: Chan Event -> (Int, (Handle, Handle)) -> IO ()
readDzen2 eventChan (n, (_, h)) = do _ <- forkIO $ forever read; return ()
  where
    read :: IO ()
    read = do
      msg <- hGetLine h
      writeChan eventChan $ DzenActivity n msg

-- dbus event listening code

dbusSetupListener :: Chan Event -> IO ()
dbusSetupListener eventChan = do
  -- connect to session bus
  dbus <- DBC.connect . fromJust =<< DB.getSessionAddress
  -- make sure we're the only instance of dzen2-update running
  nameReply <- DBC.requestName dbus ourName [DBC.nameDoNotQueue]
  case nameReply of
    DBC.NamePrimaryOwner -> DBC.listen dbus match callback
    _                    -> barf
  where
    ourName :: DB.BusName
    ourName = DB.busName_ "fi.zem.XMonad.Dzen2Update"
    match :: DBC.MatchRule
    match = DBC.matchAny {
      DBC.matchPath = DB.parseObjectPath "/fi/zem/xmonad/status",
      DBC.matchInterface = DB.parseInterfaceName "fi.zem.XMonad.Status"
      }
    callback :: DB.Signal -> IO ()
    callback sig = handle (getMemberName sig) (DB.signalBody sig)
    getMemberName :: DB.Signal -> String
    getMemberName = DB.formatMemberName . DB.signalMember
    handle :: String -> [DB.Variant] -> IO ()
    handle "StatusUpdate" [body] = writeChan eventChan . StatusUpdate $ decode body
    handle "Shutdown" _ = writeChan eventChan QuitEvent
    decode :: DB.Variant -> String
    decode = T.unpack . TE.decodeUtf8With TEE.lenientDecode . fromJust . DB.fromVariant
    barf :: IO ()
    barf = putStrLn "dzen2-update already running" >> exitFailure

-- xinerama screen query

xineramaQuery :: X.Display -> IO [((Int,Int),(Int,Int))]
xineramaQuery dpy =
  map getCoords <$> XI.getScreenInfo dpy
  where
    getCoords :: X.Rectangle -> ((Int,Int),(Int,Int))
    getCoords r = ((fromIntegral $ X.rect_x r, fromIntegral $ X.rect_y r),
                   (fromIntegral $ X.rect_width r, fromIntegral $ X.rect_height r))
