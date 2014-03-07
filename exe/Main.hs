
module Main where

import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import qualified CabalBounds.Args as Args
import CabalBounds.Main (cabalBounds)

main :: IO ()
main = do
  args  <- Args.get
  error <- cabalBounds args
  case error of
       Just err -> hPutStrLn stderr ("cabal-bounds: " ++ err) >> exitFailure
       _        -> exitSuccess
