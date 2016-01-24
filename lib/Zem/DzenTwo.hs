module Zem.DzenTwo
  ( D2Text(..)
  , readDzen2
  , startDzen2
  ) where

import Control.Concurrent
import Control.Monad
import System.IO
import System.Process

-- dzen2 formatting language

data D2Text =
  D2T [D2Text]
  | D2Lit String
  | D2Raw String
  | D2Color (String, String) D2Text
  | D2Clickable String D2Text
  | D2Gap Int D2Text
  | D2Sep Int Int

instance Show D2Text where
  show (D2T chunks) = concat $ map show chunks
  show (D2Lit body) = dzen2Escape body
  show (D2Raw body) = body
  show (D2Color (fg, bg) body) =
    "^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")" ++ show body ++ "^fg()^bg()"
  show (D2Clickable name body) =
    "^ca(1, echo m1" ++ name ++ ")" ++ show body ++ "^ca()"
  show (D2Gap w body) =
    "^r(+" ++ show w ++ "x0)" ++ show body ++ "^r(+" ++ show w ++ "x0)"
  show (D2Sep w h) =
    "^p(+" ++ show w ++ ")^r(1x" ++ show h ++ ")^p(+" ++ show w ++ ")"

dzen2Escape :: String -> String
dzen2Escape ('^':rest) = '^' : '^' : dzen2Escape rest
dzen2Escape (c:rest) = c : dzen2Escape rest
dzen2Escape [] = []

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
