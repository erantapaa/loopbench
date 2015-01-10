{-
 - Usage:
 -
 -   runhaskell run.hs [c|h] elo ehi
 -
 - Examples:
 -
 -   # run the C program with e parameters 10 .. 20:
 -   runhaskell run.hs c 10 20
 -
 -   # run the Haskell program with e parameters 10 .. 20:
 -   runhaskell run.hs h 10 20
 -}

import System.Directory
import System.Environment
import Data.Maybe (catMaybes)
import Data.Char
import Text.Printf
import Data.List
import System.IO
import System.Process
import System.Exit
import Control.Monad

dataDir = "./data"

extractId :: String -> Maybe Int
extractId str =
  if all isDigit str then Just $ read str else Nothing

-- return the next available result path for a prefix
nextResultPath prefix = do
  -- find all files in dataDir which match "data/h-..."
  files <- getDirectoryContents dataDir
  let ids = catMaybes [ extractId (drop (length prefix) f) | f <- files, isPrefixOf prefix f ]
      nextid = maximum ([0] ++ ids) + 1
      num = printf "%03d" nextid
  return $ dataDir ++ "/" ++ prefix ++ num

runHaskell :: Int -> IO ExitCode
runHaskell e = do
  let prefix = printf "h%03d-" e
  rpath <- nextResultPath prefix
  sys $ "./h5 " ++ show e ++ " +RTS -s 2> " ++ rpath

runC e = do
  let prefix = printf "c%03d-" e
  rpath <- nextResultPath prefix
  sys $ "(time ./c5 " ++ show e ++ ") > " ++ rpath ++ " 2>&1"

sys :: String -> IO ExitCode
sys cmd = do
  hPutStrLn stderr $ "+ " ++ cmd
  system cmd

main = do
  args <- getArgs
  let go kind los his = maybe (error "bad params") (\(r,lo,hi) -> forM_ [lo..hi] r) mTuple
        where
          mTuple = do
            lo <- extractId los
            hi <- extractId his
            r <- case kind of
                  "c" -> return runC
                  "h" -> return runHaskell
                  _   -> Nothing
            return (r, lo, hi)

  case args of
    (kind:lo:hi:_) -> go kind lo hi
    (kind:lo:_)    -> go kind lo lo
    _              -> error "bad usage"

