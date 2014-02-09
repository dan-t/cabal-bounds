
module Main where

import qualified CabalBounds.Args as Args
import CabalBounds.Main (cabalBounds)

main :: IO ()
main = Args.get >>= cabalBounds
