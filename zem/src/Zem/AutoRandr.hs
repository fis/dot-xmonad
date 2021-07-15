-- |
-- Module : Zem.AutoRandr
--
-- TODO describe.
module Zem.AutoRandr
  ( -- * Usage
    -- $usage
    autoRandr
  , autoRandrEventHook
  , autoRandrApply
  , OutputMatcher
  , output
  , monitor
  , serial
  , size
  , validSize
  , (<&&>)
  , Output(..)
  , autoRandr'
  , autoRandrEventHook'
  , autoRandrApply'
  , matchOutputs
  , callXrandr
  , xrandrStartupHook
  , xrandrScreenChangeEventHook
  , showCurrentOutputs
  ) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Monad (void, when)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Char (chr)
import Data.List (elemIndex, find, partition, nub, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Monoid (All(..))
import Data.Word (Word32)
import Foreign.C.Types (CULong)
import Graphics.X11.Xrandr (
  xrrGetOutputInfo, xrrGetOutputProperty, xrrGetScreenResources, xrrSelectInput,
  XRRModeInfo(..), XRROutputInfo(..), XRRScreenResources(..))
import Numeric (showHex)
import Text.Read (readMaybe)
import XMonad
import XMonad.Util.Run (safeSpawn)

import qualified XMonad.Util.ExtensibleState as XS

-- $usage
--
-- The simplest way to use this module is through the configuration syntax, which lets you map a list of monitors you
-- expect to see connected to a list of corresponding @xrandr@ command-line arguments.
--
-- The following highly redundant example showcases many of the features:
--
-- > myRandrConfig =
-- >   -- Explicitly specify the output and monitor names:
-- >   [ ([output "DP-4" <&&> monitor "DELL U2515H", output "DVI-D-0" <&&> monitor "L2000C"],
-- >      ["--output", "DVI-D-0", "--mode", "1600x1200",
-- >       "--output", "DP-4", "--primary", "--mode", "2560x1440", "--right-of", "DVI-D-0"])
-- >   -- Use variables to set up the configuration based on monitor names alone:
-- >   , ([monitor "DELL U2515H", monitor "L2000C"],
-- >      ["--output", "$1", "--mode", "1600x1200",
-- >       "--output", "$0", "--primary", "--mode", "2560x1440", "--right-of", "$1"])
-- >   -- Use the preferred resolutions of the monitors to differentiate:
-- >   , ([size (2560, 1440), size (1600, 1200)],
-- >      ["--output", "$1", "--mode", "1600x1200",
-- >       "--output", "$0", "--primary", "--mode", "2560x1440", "--right-of", "$1"])
-- >   ]
-- >
-- > -- If no configuration matched, set up outputs automatically in this order (left to right).
-- > myRandrFallback = ["DVI-D-0", "DP-0"]
-- >
-- > main = xmonad . autoRandr myRandrConfig myRandrFallback $ ...
-- >
-- > -- Optionally, include a key binding to force applying the config:
-- > myKeys = [("M-S-r", autoRandrApply myRandrConfig myRandrFallback True), ...]
--
-- Here's an example configuration for telling a laptop to use its internal display when no external displays are
-- configured, or turn off the internal display when the two external monitors are connected. It will not change the
-- configuration otherwise, leaving it for you to do manually (e.g. with @xrandr@ or @arandr@).
--
-- > myRandrConfig =
-- >   [ ([int],
-- >      ["--size", "1920x1080", "--output", "$0", "--mode", "1920x1080", "--pos", "0x0", "--primary"])
-- >   , ([int, extL, extR],
-- >      ["--size", "5120x1440",
-- >       "--output", "$0", "--off",
-- >       "--output", "$1", "--mode", "2560x1440", "--pos", "0x0",
-- >       "--output", "$2", "--mode", "2560x1440", "--pos", "2560x0", "--primary"])
-- >   ]
-- >   where
-- >     int  = output "eDP-1"
-- >     extL = monitor "DELL U2515H"
-- >     extR = monitor "DELL U2520D"
-- >
-- > myRandrFallback = []
--
-- Whenever the set of connected monitors change, the first configuration where the output/monitor specifications can be
-- matched one-to-one to the new connected outputs is selected, and the corresponding arguments are passed to @xrandr@.
-- In the argument list, you can use @$0@, @$1@, ... to refer to the output name assigned to the given matcher. The
-- system will automatically add @--output X --off@ arguments for all the disconnected outputs, but what you provide
-- should set the configuration for all the connected ones.
--
-- The monitor names used by the 'monitor' matcher are figured out from the EDID data. If the EDID data does not contain
-- a name descriptor, the manufacturer, product and serial numbers are used instead (as with the 'serial' matcher). If
-- even those are unavailable, the monitor name will be just "UNAVAILABLE", and you'll need to use other matchers. The
-- following command can be used to figure out how your currently connected monitors are named by this module:
--
-- > ghc -e Zem.AutoRandr.showCurrentOutputs
-- > # or if you're building with stack
-- > stack ghc -- -e Zem.AutoRandr.showCurrentOutputs
--
-- If there is no matching configuration, but all the connected output names can be found in the fallback list, the
-- outputs will be automatically configured to use the preferred resolution, and positioned left-to-right in the order
-- matching the list (omitting any outputs not currently connected). This means that in simple cases you can simply use
-- @[]@ as the configuration, and only provide the fallback list.
--
-- If you want to hook the code to your configuration manually, instead of using the 'autoRandr' function, you must
-- include 'xrandrStartupHook' to your @startupHook@ and 'autoRandrEventHook' in your @handleEventHook@.
--
-- You might also consider adding a key binding for 'autoRandrApply', which you can use to refresh the @xrandr@
-- configuration even if no changes were detected. With the @force@ option set to @True@, you can also use that key to
-- recover if you have manually changed your Xrandr configuration. When not forced, the module will never pass the same
-- set of arguments to @xrandr@ consecutively.
--
-- For more flexibility, you can use 'autoRandr'' (or 'autoRandrEventHook'' and 'autoRandrApply'') to specify your own
-- custom function. It will be called on any screen change event with the list of all outputs, and can do whatever it
-- wants as a reaction. If you opt to use this, you might find the 'matchOutputs' and 'callXrandr' functions useful as
-- building blocks.
--
-- Finally, you can use 'xrandrScreenChangeEventHook' to just get called on any screen change notification, with no
-- automatic Xrandr output querying.

-- | Applies the AutoRandr hooks to a XMonad configuration. See the Usage section for details.
autoRandr :: [([OutputMatcher], [String])] -> [String] -> XConfig a -> XConfig a
autoRandr ac fc xc = autoRandr' (applyConfig ac fc False) xc

-- | Event hook for applying the AutoRandr configuration when Xrandr screen changes are detected. If you're not using
-- the 'autoRandr' function, you'll need to include this in your @handleEventHook@. Note that it will never be called
-- unless you also call the 'xrandrStartupHook'.
autoRandrEventHook :: [([OutputMatcher], [String])] -> [String] -> Event -> X All
autoRandrEventHook ac fc = autoRandrEventHook' $ applyConfig ac fc False

-- | Applies the AutoRandr configuration to the currently configured outputs. If the third parameter is @True@, the
-- matching @xrandr@ command (if any) is executed even if the arguments match the ones most recently used.
autoRandrApply :: [([OutputMatcher], [String])] -> [String] -> Bool -> X ()
autoRandrApply ac fc force = autoRandrApply' $ applyConfig ac fc force

-- | Type for describing whether an 'Output' record is what you were looking for. You can use any function, but the
-- provided 'output', 'monitor', 'serial', 'size' and 'validSize' matchers cover a lot of ground. Matchers can be
-- combined with the '<&&>' operator, like @ManageHook@s.
type OutputMatcher = Output -> Bool

-- | Matches if the Xrandr output name (e.g., @DP-1@ or @HDMI-0@) is equal to the provided string.
output :: String -> OutputMatcher
output = outputTest outputName (==)

-- | Matches if the monitor name in the EDID display description (e.g., "DELL U2515H") is equal to the provided
-- string. If the monitor name is not available, matches against the serial instead. You can use the
-- 'showCurrentOutputs' function to figure out what your connected monitors are called.
monitor :: String -> OutputMatcher
monitor = outputTest outputMonitor (==)

-- | Matches if the monitor EDID data (e.g. "DEL:d06f:01020304") is equal to the provided string. You can use the
-- 'showCurrentOutputs' function to figure out what your connected monitor number serial numbers are.
serial :: String -> OutputMatcher
serial = outputTest outputSerial (==)

-- | Matches if the preferred resolution (width, height) of the monitor is equal to the provided value.
size :: (Int, Int) -> OutputMatcher
size = outputTest outputSize (==)

-- | Matches if the provided resolution (width, height) is on the list of valid modes for the monitor.
validSize :: (Int, Int) -> OutputMatcher
validSize = outputTest outputSizes elem

-- Combines one of the output property accessors with a test function to make a matcher.
outputTest :: (Output -> a) -> (b -> a -> Bool) -> b -> Output -> Bool
outputTest p t x = t x . p

-- | Represents information Xrandr has about a potential display output.
data Output = Output { -- | Name of the output, e.g. @DP-1@ or @HDMI-0@.
                       outputName :: String
                       -- | @True@ if the output seems to be connected to a monitor. If @False@, the remaining fields do
                       -- not have meaningful values.
                     , outputConnected :: Bool
                       -- | The preferred resolution (or just an arbitrary one, if the driver doesn't prefer any) of the
                       -- connected monitor.
                     , outputSize :: (Int, Int)
                       -- | The full list of valid modes for the monitor, deduplicated.
                     , outputSizes :: [(Int, Int)]
                       -- | The monitor name from the EDID display description (e.g., "DELL U2515H"), if available. If
                       -- not, the serial number. Or just the word @UNAVAILABLE@, if not even that could be found.
                     , outputMonitor :: String
                       -- | The monitor serial number details from EDID (e.g., "DEL:d06f:01020304").
                     , outputSerial :: String
                     } deriving (Show)

-- | Applies the AutoRandr hooks but with a custom handler function.
--
-- Note that the list of outputs includes all the outputs, even disconnected ones: filter the list by 'outputConnected'
-- if you need to.
--
-- Also note that the handler function may get called spuriously, as Xrandr may report screen changes that aren't really
-- relevant. There's some automatic debouncing (action is delayed by half a second, and new notifications during the
-- delay time are suppressed), but you should probably still not do anything terribly expensive unconditionally. The
-- 'callXrandr' function might help if you're planning to call the @xrandr@ binary in your handler function.
autoRandr' :: ([Output] -> X ()) -> XConfig a -> XConfig a
autoRandr' f c = c { startupHook = xrandrStartupHook <+> startupHook c
                   , handleEventHook = autoRandrEventHook' f <+> handleEventHook c }

-- | Event hook for applying the given custom handler whenever Xrandr screen changes are reported. See 'autoRandr'' for
-- notes on the handler. No screen change events are detected unless you also call 'xrandrStartupHook' beforehand. This
-- hook also handles delaying the reaction by a bit, and suppressing new change events that happen during the delay, to
-- avoid triggering the action more often than necessary.
autoRandrEventHook' :: ([Output] -> X ()) -> Event -> X All
autoRandrEventHook' _ (RRScreenChangeNotifyEvent {}) = do
  debouncingCheck . xfork $ do
    dpy <- openDisplay ""
    threadDelay 500000
    a <- internAtom dpy "XMONAD_AUTORANDR_TRIGGER" False
    allocaXEvent $ \e -> do
      setEventType e clientMessage
      let rw = defaultRootWindow dpy
      setClientMessageEvent e rw a 32 0 currentTime
      sendEvent dpy rw False structureNotifyMask e
  return $ All True
autoRandrEventHook' f (ClientMessageEvent {ev_message_type = mt}) = do
  a <- getAtom "XMONAD_AUTORANDR_TRIGGER"
  when (mt == a) $ debouncingClear >> autoRandrApply' f
  return $ All True
autoRandrEventHook' _ _ = return $ All True

-- | Applies the custom handler to the current set of connected Xrandr outputs.
autoRandrApply' :: ([Output] -> X ()) -> X ()
autoRandrApply' = (wrapIO queryOutputs >>=)

-- | Startup hook that requests Xrandr screen change notifications to be delivered to Xmonad. If this (or something
-- equivalent) is not called in the startup, the event hooks will not work either. Normally the 'autoRandr' (or
-- 'autoRandr'') function inserts this into your configuration's startup hooks.
xrandrStartupHook :: X ()
xrandrStartupHook = wrapIO $ \dpy root -> xrrSelectInput dpy root rrScreenChangeNotifyMask

-- | Converts the given action to an event hook that reacts to Xrandr screen change notification events only. This is
-- the low-level interface that does no automatic querying of Xrandr outputs for you. You still also need the
-- 'xrandrStartupHook' for this to work.
xrandrScreenChangeEventHook :: X All -> Event -> X All
xrandrScreenChangeEventHook hook (RRScreenChangeNotifyEvent {}) = hook
xrandrScreenChangeEventHook _ _ = return $ All True

-- Turns the 'autoRandr' config syntax into an 'autoRandr'' handler function.
applyConfig :: [([OutputMatcher], [String])] -> [String] -> Bool -> [Output] -> X ()
applyConfig conf order force outputs = xrandr $ lookupConfig <|> fallback
  where
    -- Happy case: find a specific list of arguments for the set of connected outputs & monitors.
    lookupConfig :: Maybe [String]
    lookupConfig = matchConfig conf connected
    -- Fallback configuration: set up connected outputs in auto mode left-to-right based on their order in the list.
    fallback :: Maybe [String]
    fallback =
      case map ((`elemIndex` order) &&& id) . names $ connected of
        indices | all (isJust . fst) indices -> Just . fallbackArgs . fallbackOutputs $ indices
        _ -> Nothing
    fallbackOutputs :: [(Maybe Int, String)] -> [String]
    fallbackOutputs indices = map snd . sortOn (\(Just index, _) -> index) $ indices
    fallbackArgs :: [String] -> [String]
    fallbackArgs outs = concat $ zipWith fallbackArg outs ("":outs)
    fallbackArg :: String -> String -> [String]
    fallbackArg n "" = ["--output", n, "--auto"]
    fallbackArg n p = ["--output", n, "--auto", "--right-of", p]
    -- Call xrandr with the output args + --off for all disconnected outputs.
    xrandr :: Maybe [String] -> X ()
    xrandr (Just args@(_:_)) = callXrandr force $ args ++ offArgs
    xrandr _ = return ()
    offArgs :: [String]
    offArgs = concat . map offArg . names $ disconnected
    offArg :: String -> [String]
    offArg name = ["--output", name, "--off"]
    -- Disconnected and connected outputs and names.
    connected :: [Output]
    connected = filter outputConnected outputs
    disconnected :: [Output]
    disconnected = filter (not . outputConnected) outputs
    names :: [Output] -> [String]
    names = map outputName

-- Finds the matching config line, and expands any variables in the xargs arguments.
matchConfig :: [([OutputMatcher], [String])] -> [Output] -> Maybe [String]
matchConfig conf = fmap expandArgs . matchOutputs conf
  where
    expandArgs :: ([String], [Output]) -> [String]
    expandArgs (args, outs) = map (expand outs) args
    expand :: [Output] -> String -> String
    expand outs arg@('$':num) = case readMaybe num :: Maybe Int of
      Just n
        | n >= 0 && n < length outs -> outputName . (!! n) $ outs
        | otherwise -> arg
      Nothing -> arg
    expand _ arg = arg

-- | Finds the first matching configuration for the given list of outputs, and returns the associated element (e.g.,
-- @xrandr@ arguments for the standard configuration format) together with the selected outputs for each matcher, in the
-- same order as listed in the configuration.
matchOutputs :: [([OutputMatcher], a)] -> [Output] -> Maybe (a, [Output])
matchOutputs ((spec, args):conf) conn
  | length spec == length conn =
    case match spec conn of
      Just matched -> Just (args, matched)
      _ -> matchOutputs conf conn
  | otherwise = matchOutputs conf conn
  where
    -- Figures how (and if) a list of output matchers can match a list of outputs, and returns the corresponding matched
    -- outputs in the same order as the matchers.
    match :: [OutputMatcher] -> [Output] -> Maybe [Output]
    match (m:ms) rest = try ms $ partition m rest
    match _ _ = Nothing
    try :: [OutputMatcher] -> ([Output], [Output]) -> Maybe [Output]
    try [] ((good:[]), []) = Just [good]
    try [] _ = Nothing
    try ms ((good:goods), bads) = case match ms (goods ++ bads) of
      Just matched -> Just $ good:matched
      Nothing -> try ms (goods, good:bads)
    try _ _ = Nothing
matchOutputs [] _ = Nothing

-- | Calls the @xrandr@ binary with the given list of arguments. If the first parameter is @True@, the command will be
-- called unconditionally. Otherwise, the command will only be called if the list of arguments is different from the
-- previous call to this function.
callXrandr :: Bool -> [String] -> X ()
callXrandr force args = do
  modified <- xrandrArgsModified args
  when (modified || force) $ safeSpawn "xrandr" args
-- TODO: think about adding a delay for debouncing...

-- | Prints out details of the currently connected Xrandr outputs. You can likely use a command like the following, from
-- a shell command line, to show your current outputs:
--
-- > $ ghc -e Zem.AutoRandr.showCurrentOutputs
-- > Output "DVI-D-0": disconnected
-- > ...
-- > Output "DP-4":
-- >   Monitor: "DELL U2515H"
-- >   Serial: "DEL:d06f:01020304"
-- >   Preferred size: (2560,1440)
-- >   Valid sizes: [(2560,1440),(2048,1280),...]
showCurrentOutputs :: IO ()
showCurrentOutputs = do
  dpy <- openDisplay ""
  queryOutputs dpy (defaultRootWindow dpy) >>= mapM_ showOutput
  where
    showOutput :: Output -> IO ()
    showOutput (Output { outputName = n, outputConnected = False }) =
      putStrLn $ "Output " ++ (show n) ++ ": disconnected"
    showOutput (Output { outputName = n, outputConnected = True, outputSize = s, outputSizes = ss, outputMonitor = m, outputSerial = ser }) = do
      putStrLn $ "Output " ++ (show n) ++ ":"
      putStrLn $ "  Monitor: " ++ (show m)
      putStrLn $ "  Serial: " ++ (show ser)
      putStrLn $ "  Preferred size: " ++ (show s)
      putStrLn $ "  Valid sizes: " ++ (show ss)

-- Extracts descriptions of all outputs reported by Xrandr.
queryOutputs :: Display -> Window -> IO [Output]
queryOutputs dpy root = xrrGetScreenResources dpy root >>= describeOutputs dpy

-- Parses the Xrandr screen resources and describes all the outputs in it.
describeOutputs :: Display -> Maybe XRRScreenResources -> IO [Output]
describeOutputs dpy (Just (res@XRRScreenResources {xrr_sr_outputs = outputs})) =
  mapM (describeOutput dpy res) outputs >>= return . catMaybes
describeOutputs _ _ = return []

-- Queries Xrandr for the details of a single output, and formats it into an Output.
describeOutput :: Display -> XRRScreenResources -> RROutput -> IO (Maybe Output)
describeOutput dpy res out = do
  info <- describeOutputInfo res <$> xrrGetOutputInfo dpy res out
  edid <- io (readEDID dpy out)
  return $ merge info edid
  where
    merge :: Maybe (String, Bool, [(Int, Int)]) -> (String, String) -> Maybe Output
    merge (Just (name, conn, modes)) (mon, ser) =
      Just (Output { outputName = name
                   , outputConnected = conn
                   , outputSize = fromMaybe (0, 0) . listToMaybe $ modes
                   , outputSizes = nub modes
                   , outputMonitor = mon
                   , outputSerial = ser
                   })
    merge _ _ = Nothing

-- Decompoes a XRROutputInfo into (name, connected-flag, [sizes]) structure.
describeOutputInfo :: XRRScreenResources -> Maybe XRROutputInfo -> Maybe (String, Bool, [(Int, Int)])
describeOutputInfo res (Just out)
  | XRRScreenResources {xrr_sr_modes = allModes} <- res
  , XRROutputInfo {xrr_oi_connection = conn, xrr_oi_name = name, xrr_oi_modes = modes} <- out
  = Just (name, conn == 0, mapMaybe (lookupSize allModes) $ modes)
  where
    lookupSize :: [XRRModeInfo] -> RRMode -> Maybe (Int, Int)
    lookupSize modes mode = getSize <$> find (\(XRRModeInfo {xrr_mi_id = i}) -> i == mode) modes
    getSize :: XRRModeInfo -> (Int, Int)
    getSize (XRRModeInfo {xrr_mi_width = w, xrr_mi_height = h}) = (fromIntegral w, fromIntegral h)
describeOutputInfo _ _ = Nothing

-- Reads the monitor name and serial number from the Xrandr EDID property.
readEDID :: Display -> RROutput -> IO (String, String)
readEDID dpy out = do
  aEDID <- internAtom dpy "EDID" False
  parseEDID <$> xrrGetOutputProperty dpy out aEDID 0 256 False False anyPropertyType

-- Parses the EDID property into (name, serial).
parseEDID :: Maybe (Atom, Int, CULong, [Word32]) -> (String, String)
parseEDID (Just (_, 8, _, bytes))
  | length bytes >= 128 = formatEDID bytes
  | otherwise = let err = "TRUNCATED:" ++ (show $ length bytes) in (err, err)
parseEDID _ = ("UNAVAILABLE", "UNAVAILABLE")

-- Parses the EDID binary blob into (name, serial).
formatEDID :: [Word32] -> (String, String)
formatEDID bytes = (head $ monitorNames ++ [serialName], serialName)
  where
    monitorNames :: [String]
    monitorNames = mapMaybe (monitorName . slice bytes) $ [(54, 72), (72, 90), (90, 108), (108, 126)]
    monitorName :: [Word32] -> Maybe String
    monitorName desc
      | slice desc (0, 4) == [0, 0, 0, 252] = Just . map (chr . fromIntegral) . takeWhile (/= 10) . drop 5 $ desc
      | otherwise = Nothing
    serialName :: String
    serialName = raw $ slice bytes (8, 16)
    raw :: [Word32] -> String
    raw [m1, m2, p1, p2, s1, s2, s3, s4] =
      manufacturer ((m1 `shiftL` 8) .|. m2) ++ padHex 4 [p1, p2] ++ padHex 8 [s1, s2, s3, s4]
    raw _ = "IMPOSSIBLE"
    manufacturer :: Word32 -> String
    manufacturer m = map letter [(m `shiftR` 10), ((m `shiftR` 5) .&. 31), (m .&. 31)]
    letter :: Word32 -> Char
    letter n
      | n >= 1 && n <= 26 = chr . fromIntegral $ 64 + n
      | otherwise = '?'
    padHex :: Int -> [Word32] -> String
    padHex w n =
      let hex = showHex (le n) ""
      in  ":" ++ take (w - length hex) ['0','0'..] ++ hex
    le :: [Word32] -> Word32
    le = foldr (\b n -> (n `shiftL` 8) .|. b) 0
    slice :: [Word32] -> (Int, Int) -> [Word32]
    slice xs (from, to) = take (to-from) . drop from $ xs

-- Lifts an IO to X and automatically provides it the display and root.
wrapIO :: (Display -> Window -> IO a) -> X a
wrapIO f = do
  dpy <- asks display
  root <- asks theRoot
  io $ f dpy root

-- State for storing the de-bouncing timer state flag.

data DebouncingFlag = DebouncingIdle | DebouncingActive deriving (Eq, Typeable)

instance ExtensionClass DebouncingFlag where
  initialValue = DebouncingIdle

debouncingCheck :: X a -> X ()
debouncingCheck f = XS.modified (const DebouncingActive) >>= (`when` void f) >> return ()

debouncingClear :: X ()
debouncingClear = XS.put DebouncingIdle

-- Persistent state for storing the last xrandr command arguments.

data LastCallArgs = LastCallArgs [String] deriving (Eq, Show, Read, Typeable)

instance ExtensionClass LastCallArgs where
  initialValue = LastCallArgs []
  extensionType = PersistentExtension

xrandrArgsModified :: [String] -> X Bool
xrandrArgsModified = XS.modified . const . LastCallArgs
