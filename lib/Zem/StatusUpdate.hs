module Zem.StatusUpdate
  ( WSType(..)
  , WS(..)
  , StatusUpdate(..)
  , packUpdate
  , unpackUpdate
  ) where

import Data.Int
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified DBus as DB

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

packUpdate :: StatusUpdate -> DB.Variant
packUpdate up = DB.toVariant (screen, ws, updLayout up, title)
  where screen = toEnum (updScreen up) :: Int32
        ws = map packWS $ updWS up
        title = TE.encodeUtf8 . T.pack $ updTitle up
        packWS :: (String, WSType, Int, Bool) -> (String, Int32, Int32, Bool)
        packWS (n, t, s, u) = (n, toEnum . fromEnum $ t, toEnum s, u)

unpackUpdate :: DB.Variant -> Maybe StatusUpdate
unpackUpdate up = fmap unpack $ DB.fromVariant up
  where
    unpack :: (Int32, [(String, Int32, Int32, Bool)], String, B.ByteString) -> StatusUpdate
    unpack (scr, ws, layout, title) =
      StatusUpdate { updScreen = fromEnum scr
                   , updWS = map unpackWS ws
                   , updLayout = layout
                   , updTitle = if B.null title then "" else T.unpack . TE.decodeUtf8With TEE.lenientDecode $ title
                   }
    unpackWS :: (String, Int32, Int32, Bool) -> (String, WSType, Int, Bool)
    unpackWS (n, t, s, u) = (n, toEnum . fromEnum $ t, fromEnum s, u)
