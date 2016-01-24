module Zem.DzenTwo
  ( dzen2Clickable
  , dzen2Color
  , dzen2Gap
  , dzen2Sep
  , readDzen2
  , startDzen2
  ) where

import Control.Concurrent
import Control.Monad
import System.IO
import System.Process

-- dzen2 formatting helpers

dzen2Clickable :: String -> String -> String
dzen2Clickable name str =
  "^ca(1, echo m1" ++ name ++ ")" ++ str ++ "^ca()"

dzen2Color :: (String, String) -> String -> String
dzen2Color (fg, bg) str =
  "^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")" ++ str ++ "^fg()^bg()"

dzen2Gap :: (Int, Int) -> String -> String
dzen2Gap (front, back) str =
  "^r(+" ++ show front ++ "x0)" ++ str ++ "^r(+" ++ show back ++ "x0)"

dzen2Sep :: Int -> Int -> String
dzen2Sep space height = "^p(+" ++ show space ++ ")^r(1x" ++ show height ++ ")^p(+" ++ show space ++ "4)"

-- dzen2 process management

startDzen2 :: String -> ((Int,Int),(Int,Int)) -> (String,String) -> String -> IO (Handle, Handle)
startDzen2 bin ((x, y), (w, h)) (fg, bg) font = do
  (inp, out, _, _) <- runInteractiveProcess bin args Nothing Nothing
  hSetBuffering inp LineBuffering
  hSetBuffering out LineBuffering
  hPutStrLn inp "(dzen2)"
  return (inp, out)
  where
    args :: [String]
    args =
      ["-ta", "l",
       "-x", show x, "-y", show y, "-w", show w, "-h", show h,
       "-fg", fg, "-bg", bg,
       "-fn", font,
       "-e", "button3=print:m3"
      ]

readDzen2 :: Handle -> (String -> IO ()) -> IO ()
readDzen2 h callback = do _ <- forkIO $ forever read; return ()
  where
    read :: IO ()
    read = hGetLine h >>= callback

-- readDzen2 :: Chan Event -> (Int, (Handle, Handle)) -> IO ()
-- readDzen2 eventChan (n, (_, h)) = do _ <- forkIO $ forever read; return ()
--   where
--     read :: IO ()
--     read = do
--       msg <- hGetLine h
--       writeChan eventChan $ DzenActivity n msg
