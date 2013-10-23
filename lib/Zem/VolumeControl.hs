{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Zem.VolumeControl (
  getControlVolume, setControlVolume, adjustControlVolume,
  toggleControlMute,
  getVolumes, getVolume, setVolume, adjustVolume,
  toggleMute,
  adjustVolumeAndNotify,
  toggleMuteAndNotify
  ) where

import Data.Int
import Data.List
import Data.Map
import Data.Maybe
import Data.Time.Clock
import Data.Typeable
import Data.Word
import Sound.ALSA.Mixer
import Control.Concurrent
import Control.Monad

import qualified Data.Traversable as T

import qualified DBus as DB
import qualified DBus.Client as DBC

import XMonad.Core
import qualified XMonad.Util.ExtensibleState as XS

-- volume adjustment of single control

doControlVolume :: (Volume -> IO b) -> Control -> IO (Maybe b)
doControlVolume act ctrl = T.for (playback $ volume ctrl) act

getControlVolume :: Control -> IO (Maybe ([Integer], Integer, Integer))
getControlVolume = doControlVolume $ \vol -> do
  (min, max) <- getRange vol
  vols <- mapM (flip getChannel $ value vol) (channels $ value vol)
  return (catMaybes vols, min, max)

setControlVolume :: Integer -> Control -> IO ()
setControlVolume newVol =
  void . (doControlVolume $ \vol ->
           mapM_ (\ch -> setChannel ch (value vol) newVol) (channels $ value vol))

adjustControlVolume :: Integer -> Control -> IO (Maybe (Integer, Integer, Integer))
adjustControlVolume offset = doControlVolume $ \vol -> do
  (vMin, vMax) <- getRange vol
  maybeVols <- mapM (flip getChannel $ value vol) (channels $ value vol)
  let vols = catMaybes maybeVols
      newVol = min vMax . max vMin $ (sum vols `div` genericLength vols) + offset
  mapM_ (\ch -> setChannel ch (value vol) newVol) (channels $ value vol)
  return (newVol, vMin, vMax)

toggleControlMute :: Control -> IO (Maybe Bool)
toggleControlMute ctrl = fmap join $ T.for (playback $ switch ctrl) toggle
  where
    toggle sw =
      getChannel FrontLeft sw >>= T.traverse (toggleChans sw)
    toggleChans sw state =
      mapM_ (\ch -> setChannel ch sw (not state)) (channels sw) >> return (not state)

-- volume adjustment of the default/Master control

withMasterControl :: (Control -> IO a) -> a -> IO a
withMasterControl f def =
  fmap (fromMaybe def) $ getControlByName "default" "Master" >>= T.traverse f

getVolumes :: IO (Maybe ([Integer], Integer, Integer))
getVolumes = withMasterControl getControlVolume Nothing

getVolume :: IO (Maybe (Integer, Integer, Integer))
getVolume = fmap (fmap (\(vols, min, max) -> (sum vols `div` genericLength vols, min, max))) getVolumes

setVolume :: Integer -> IO ()
setVolume newVol = withMasterControl (setControlVolume newVol) ()

adjustVolume :: Integer -> IO (Maybe (Integer, Integer, Integer))
adjustVolume offset = withMasterControl (adjustControlVolume offset) Nothing

toggleMute :: IO (Maybe Bool)
toggleMute = withMasterControl toggleControlMute Nothing

-- dbus notification support

data VolumeNotificationState =
  VolumeNotificationState { lastNotification :: Maybe (Word32, UTCTime) }
  deriving (Typeable, Show)

instance ExtensionClass VolumeNotificationState where
  initialValue = VolumeNotificationState { lastNotification = Nothing }

performAndNotify dbus act fmt = do
  state <- XS.get :: X (VolumeNotificationState)
  currentTime <- io getCurrentTime
  let closeTime = addUTCTime 5 currentTime
  res <- io act
  case res of
    Just result ->
      io (DBC.call_ dbus method) >>= updateState
      where
        mc = DB.methodCall "/org/freedesktop/Notifications" "org.freedesktop.Notifications" "Notify"
        method = mc { DB.methodCallDestination = Just "org.freedesktop.Notifications"
                    , DB.methodCallBody = [ DB.toVariant ("XMonad" :: String)
                                          , DB.toVariant notificationId
                                          , DB.toVariant ("" :: String)
                                          , DB.toVariant (fmt result)
                                          , DB.toVariant ("" :: String)
                                          , DB.toVariant ([] :: [String])
                                          , DB.toVariant (empty :: Map String DB.Variant)
                                          , DB.toVariant (5000 :: Int32)
                                          ]
                    }
        notificationId = maybe 0 updateId $ lastNotification state
        updateId (oldId, oldClose) = if currentTime > oldClose then 0 else oldId
        updateState ret =
          case DB.fromVariant =<< (listToMaybe . DB.methodReturnBody $ ret) :: Maybe Word32 of
            Just newId -> XS.put $ state { lastNotification = Just (newId, closeTime) }
            Nothing -> return ()
    Nothing -> return ()

adjustVolumeAndNotify :: DBC.Client -> Integer -> X ()
adjustVolumeAndNotify dbus offset =
  performAndNotify dbus (adjustVolume offset) format
  where
    format :: (Integer, Integer, Integer) -> String
    format (newVol, vMin, vMax) =
      "Volume: " ++ show (((newVol - vMin) * 100) `div` (vMax - vMin))

toggleMuteAndNotify :: DBC.Client -> X ()
toggleMuteAndNotify dbus =
  performAndNotify dbus toggleMute format
  where
    format :: Bool -> String
    format enabled =
      "Mute: " ++ (if enabled then "off" else "ON")
