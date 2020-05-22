module Zem.StatusUpdate
  ( WSType(..)
  , WS(..)
  , StatusUpdate(..)
  , packUpdate
  , unpackUpdate
  , postStatus
  , postStatusUpdate
  , listenStatus
  ) where

import Data.Int
import Data.Maybe
import System.Exit

import qualified DBus as DB
import qualified DBus.Client as DBC

-- status update data types

data WSType = WSCurrent | WSVisible | WSHidden | WSEmpty
              deriving (Eq, Ord, Enum, Show)

data WS = WS
  { wsName :: String
  , wsType :: WSType
  , wsSameScreen :: Bool
  , wsUrgent :: Bool
  } deriving (Eq, Show)

data StatusUpdate = StatusUpdate
  { updScreen :: Int
  , updWS :: [(String, WSType, Int, Bool)]
  , updLayout :: String
  , updTitle :: String
  }
  deriving (Eq, Show)

-- DBus transport

packUpdate :: StatusUpdate -> [DB.Variant]
packUpdate up = [screen, ws, DB.toVariant $ updLayout up, title]
  where screen = DB.toVariant (toEnum (updScreen up) :: Int32)
        ws = DB.toVariant . map packWS $ updWS up
        title = DB.toVariant $ updTitle up
        packWS :: (String, WSType, Int, Bool) -> (String, Int32, Int32, Bool)
        packWS (n, t, s, u) = (n, toEnum . fromEnum $ t, toEnum s, u)

-- TODO: handle errors in subparams
unpackUpdate :: DB.Variant -> DB.Variant -> DB.Variant -> DB.Variant -> Maybe StatusUpdate
unpackUpdate scr ws layout title = Just $ StatusUpdate { updScreen = fromEnum . fromJust $ (DB.fromVariant scr :: Maybe Int32)
                                                       , updWS = map unpackWS . fromJust $ (DB.fromVariant ws :: Maybe [(String, Int32, Int32, Bool)])
                                                       , updLayout = fromJust (DB.fromVariant layout :: Maybe String)
                                                       , updTitle = fromJust (DB.fromVariant title :: Maybe String)
                                                       }
  where
    unpackWS :: (String, Int32, Int32, Bool) -> (String, WSType, Int, Bool)
    unpackWS (n, t, s, u) = (n, toEnum . fromEnum $ t, fromEnum s, u)

postStatus :: DBC.Client -> String -> [DB.Variant] -> IO ()
postStatus client member body = DBC.emit client sig
  where
    sig = (DB.signal path iface memName) { DB.signalBody = body }
    path = DB.objectPath_ "/fi/zem/xmonad/status"
    iface = DB.interfaceName_ "fi.zem.XMonad.Status"
    memName = DB.memberName_ member

postStatusUpdate :: DBC.Client -> StatusUpdate -> IO ()
postStatusUpdate client update = postStatus client "StatusUpdate" $ packUpdate update

listenStatus :: (String -> [DB.Variant] -> IO ()) -> IO ()
listenStatus handle = do
  -- connect to session bus
  dbus <- DBC.connect . fromJust =<< DB.getSessionAddress
  -- make sure we're the only instance of dzen2-update running
  nameReply <- DBC.requestName dbus ourName [DBC.nameDoNotQueue]
  case nameReply of
    DBC.NamePrimaryOwner -> DBC.addMatch dbus match callback >> return ()
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
    barf :: IO ()
    barf = putStrLn "dzen2-update already running" >> exitFailure
