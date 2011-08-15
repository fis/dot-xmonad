import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Ix
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO
import System.Process
import Text.Regex.Posix

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Text as T

import qualified DBus.Address as DBA
import qualified DBus.Client as DBC
import qualified DBus.Constants as DBK
import qualified DBus.Message as DBM
import qualified DBus.Types as DBT

-- screen configuration (TODO: dynamic?)

myScreens :: [((Int,Int),(Int,Int))]
myScreens = [
  ((0,0), (1920,1200)),
  ((1920,0), (1600,1200))
  ]

myDzen2 = "/usr/bin/dzen2"

myColors = Map.fromList [
  ("default", ("#808080", "#202020")),
  ("ws-visible", ("#d0d0d0", "#606060")),
  ("ws-hidden", ("#909090", "#202020")),
  ("ws-urgent", ("#ffffff", "#700000")),
  ("title", ("#d0d0d0", "#202020"))
  ]

-- trivial helpers to access the configuration

screenCount :: Int
screenCount = length myScreens

color :: String -> (String, String)
color c = Map.findWithDefault ("#ffffff", "#ff0000") c myColors

-- main application

data Event = StatusUpdate String
           | DzenActivity Int String
           deriving Show

data WSType = WSCurrent | WSVisible | WSHidden | WSEmpty
              deriving (Eq, Ord, Show)
data WS = WS String WSType Bool
          deriving Show

wsName :: WS -> String
wsName (WS name _ _) = name
wsType :: WS -> WSType
wsType (WS _ wstype _) = wstype
wsUrgent :: WS -> Bool
wsUrgent (WS _ _ urg) = urg

data BarState = BarState {
  barActiveScreen :: Int,
  barWorkspaces :: [[WS]],
  barTitles :: [String]
  }
  deriving Show

type BarIO = StateT BarState IO

main :: IO ()
main = do
  -- set up a channel for event-receiving
  eventChan <- newChan
  -- start a dzen2 process for each screen, fork a thread to get events
  dzenHandles <- mapM startDzen2 myScreens
  mapM_ (readDzen2 eventChan) $ zip [0..] dzenHandles
  -- set up DBus listener to feed the event channel
  dbusSetupListener eventChan
  -- handle events until the end
  evalStateT (forever $ handleEvents eventChan dzenHandles) initState
  where
    initState :: BarState
    initState = BarState {
      barActiveScreen = 0,
      barWorkspaces = replicate screenCount [],
      barTitles = replicate screenCount "(dzen2)"
      }

handleEvents :: Chan Event -> [(Handle, Handle)] -> BarIO ()
handleEvents chan dzen = do
  event <- liftIO $ readChan chan
  handle event
  where
    handle :: Event -> BarIO ()
    handle (StatusUpdate msg) = do
      -- update current state
      oldState <- get
      let newState = parseUpdate msg oldState
      put newState
      -- redraw necessary dzen toolbars
      let toUpdate = nub $ barActiveScreen <$> [oldState, newState]
      mapM_ update toUpdate
    update :: Int -> BarIO ()
    update idx = do
      state <- get
      liftIO $ hPutStrLn (fst (dzen !! idx)) $ makeBar state idx

parseUpdate :: String -> BarState -> BarState
parseUpdate str oldState =
  let (wsstr:(_:title)) = splitOn ";" str
      allWorkspaces = parseWorkspaces wsstr
      newActiveScreen = (fst . fromJust . find ((== WSCurrent) . wsType . snd)) allWorkspaces
      newWorkspaces = map (getWorkspaces allWorkspaces) [0..(screenCount-1)]
      newTitle = if title == [] then "" else head title in
  oldState {
    barActiveScreen = newActiveScreen,
    barWorkspaces = newWorkspaces,
    barTitles = insertTitle newActiveScreen newTitle $ barTitles oldState
    }
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
    insertTitle :: Int -> String -> [String] -> [String]
    insertTitle at title old =
      let (before, (_:after)) = splitAt at old in
      before ++ (title:after)

-- status line formatting code

makeBar :: BarState -> Int -> String
makeBar state idx = workspaces ++ " - " ++ title
  where
    workspaces :: String
    workspaces = intercalate " " $ map makeWS $ barWorkspaces state !! idx
    title :: String
    title = dzen2Color (color "title") $ barTitles state !! idx
    makeWS :: WS -> String
    makeWS (WS name wtype urg) =
      makeName wtype urg $ makeIcon wtype ++ name
    makeIcon :: WSType -> String
    makeIcon WSEmpty = "^ro(6x6)^r(2x0)"
    makeIcon _       = "^r(6x6)^r(2x0)"
    makeName :: WSType -> Bool -> String -> String
    makeName _         True = dzen2Color (color "ws-urgent") . dzen2Gap (2,2)
    makeName WSCurrent _    = dzen2Color (color "ws-visible") . dzen2Gap (2,2)
    makeName WSVisible _    = dzen2Color (color "ws-visible") . dzen2Gap (2,2)
    makeName WSHidden  _    = dzen2Color (color "ws-hidden") . dzen2Gap (2,2)
    makeName WSEmpty   _    = dzen2Color (color "ws-hidden") . dzen2Gap (2,2)

dzen2Color :: (String, String) -> String -> String
dzen2Color (fg, bg) str =
  "^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")" ++ str ++ "^fg()^bg()"

dzen2Gap :: (Int, Int) -> String -> String
dzen2Gap (front, back) str =
  "^r(+" ++ show front ++ "x0)" ++ str ++ "^r(+" ++ show back ++ "x0)"

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
       "-x", show xpos, "-y", show ypos, "-w", show width,
       "-fg", fg, "-bg", bg,
       "-fn", "DejaVu Sans:size=10"
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
  addr <- DBA.getSession
  dbus <- DBC.connect $ head . fromJust $ addr
  DBC.listen dbus match callback
  where
    match :: DBC.MatchRule
    match = DBC.MatchRule {
      DBC.matchSender = Nothing,
      DBC.matchDestination = Nothing,
      DBC.matchPath = Just . DBT.objectPath_ $ T.pack "/fi/zem/xmonad/status",
      DBC.matchInterface = Just . DBT.interfaceName_ $ T.pack "fi.zem.XMonad.Status",
      DBC.matchMember = Nothing
      }
    callback :: DBT.BusName -> DBM.Signal -> IO ()
    callback _ sig = handle (getMemberName sig) (DBM.signalBody sig)
    getMemberName :: DBM.Signal -> String
    getMemberName = T.unpack . DBT.memberNameText . DBM.signalMember
    handle :: String -> [DBT.Variant] -> IO ()
    handle "StatusUpdate" [body] = writeChan eventChan $ StatusUpdate $ (fromJust . DBT.fromVariant) body
