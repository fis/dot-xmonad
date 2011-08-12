import Control.Concurrent
import Data.Ix
import Data.Maybe
import System.IO

import qualified Data.Text as T

import qualified DBus.Address as DBA
import qualified DBus.Client as DBC
import qualified DBus.Constants as DBK
import qualified DBus.Message as DBM
import qualified DBus.Types as DBT

-- screen configuration (TODO: dynamic?)

myScreens = [
  ((0,0), (1920,1200)),
  ((1920,0), (1600,1200))
  ]

-- code

main :: IO ()
main = do
  dbusSetupListener
  threadDelay 10000000

dbusSetupListener :: IO ()
dbusSetupListener = do
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
    handle "StatusUpdate" body = putStrLn $ show body
