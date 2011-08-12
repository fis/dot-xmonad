import Control.Concurrent
import Control.Monad
import Data.Ix
import Data.Maybe
import System.IO
import System.Process

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

-- main application

main :: IO ()
main = do
  -- set up a channel for event-receiving
  eventChan <- newChan
  -- start a dzen2 process for each screen, fork a thread to get events
  dzenHandles <- mapM startDzen2 myScreens
  mapM_ (readDzen2 eventChan) dzenHandles
  -- set up DBus listener to feed the event channel
  dbusSetupListener eventChan
  -- handle events until the end
  handleEvents eventChan dzenHandles
  where
    handleEvents :: Chan String -> [(Handle, Handle)] -> IO ()
    handleEvents chan dzen = do
      event <- readChan chan
      handle event
      handleEvents chan dzen
      where
        handle :: String -> IO ()
        -- just for testing:
        handle = hPutStrLn (fst (dzen !! 0))

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
    args = ["-ta", "l", "-x", show xpos, "-y", show ypos, "-w", show width, "-h", "16"]

readDzen2 :: Chan String -> (Handle, Handle) -> IO ()
readDzen2 eventChan (_, h) = do _ <- forkIO $ forever read; return ()
  where
    read :: IO ()
    read = do
      msg <- hGetLine h
      writeChan eventChan msg

-- dbus event listening code

dbusSetupListener :: Chan String -> IO ()
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
    handle "StatusUpdate" body = writeChan eventChan $ show body
