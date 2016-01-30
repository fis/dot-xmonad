module Zem.Utils
  ( manageAndroidStudioPopups
  ) where

import Control.Monad
import Data.Maybe
import XMonad
import XMonad.Hooks.ManageHelpers

-- Miscallaneous XMonad utility functions to keep xmonad.hs clean(er).

-- Android Studio / IntelliJ focus kludge:
-- Avoid irritating focus switches from IntelliJ on-screen popups to the main IntelliJ window that
-- result from the popup changing size so that the mouse is no longer in it, by warping the pointer
-- to the top-left corner of the popup whenever a new popup opens and the mouse is in the IntelliJ
-- main window.

manageAndroidStudioPopups :: ManageHook
manageAndroidStudioPopups =
  isStudioWindow <&&> fmap isJust transientTo <&&> title =? " " --> doX mouseToTopLeft

isStudioWindow :: Query Bool
isStudioWindow = className =? "jetbrains-studio"

doX :: (Window -> X a) -> ManageHook
doX f = (>> doF id) . liftX . f =<< ask

mouseToTopLeft :: Window -> X ()
mouseToTopLeft newWindow = do
  dpy <- asks display
  root <- asks theRoot
  (_, _, mouseWindow, _, _, _, _, _) <- io $ queryPointer dpy root
  when (mouseWindow /= none) $ do
    isStudio <- runQuery isStudioWindow mouseWindow
    when isStudio $
      io $ warpPointer dpy none newWindow 0 0 0 0 0 0
