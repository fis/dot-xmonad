{-# LANGUAGE DeriveDataTypeable #-}

module Zem.XkbSwitch
  ( switchKeyboardLayout
  ) where

import Control.Monad (when)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.X11.Xlib.Types (Display(..))

import XMonad.Actions.GridSelect
import XMonad.Core
import qualified XMonad.Util.ExtensibleState as XS

newtype XklEngine = XklEngine (Ptr XklEngine)

foreign import ccall "xkl_engine_get_instance" c_xkl_engine_get_instance :: Display -> IO XklEngine
foreign import ccall "xkl_engine_get_groups_names" c_xkl_engine_get_groups_names :: XklEngine -> Ptr CString
foreign import ccall "xkl_engine_lock_group" c_xkl_engine_lock_group :: XklEngine -> CInt -> IO ()

data XkbSwitchState =
  XkbSwitchState { engine :: Maybe XklEngine }
  deriving Typeable

instance ExtensionClass XkbSwitchState where
  initialValue = XkbSwitchState { engine = Nothing }

xklEngine :: X (XklEngine)
xklEngine = (XS.get :: X (XkbSwitchState)) >>= getOrUpdate
  where
    getOrUpdate :: XkbSwitchState -> X (XklEngine)
    getOrUpdate state = maybe initialize return (engine state)
    initialize :: X (XklEngine)
    initialize = withDisplay (io . c_xkl_engine_get_instance) >>= save
    save :: XklEngine -> X (XklEngine)
    save engine = XS.put (XkbSwitchState { engine = Just engine }) >> return engine

xklGroups :: X ([String])
xklGroups = xklEngine >>= io . peekArray0 nullPtr . c_xkl_engine_get_groups_names >>= io . mapM peekCString

selectKeyboardLayout :: X ()
selectKeyboardLayout = do
  engine <- xklEngine
  groups <- xklGroups
  let actions = zip groups (map (io . c_xkl_engine_lock_group engine) [0..])
  runSelectedAction def actions

switchKeyboardLayout :: Int -> X ()
switchKeyboardLayout group = xklEngine >>= io . flip c_xkl_engine_lock_group (toEnum group)
