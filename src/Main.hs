
module Main where

import Control.Applicative ((<$>))
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Verbosity (silent)
import qualified CabalBounds.Args as Args
import CabalBounds.Command (command)
import CabalBounds.Execute (execute)

main :: IO ()
main = do
   args     <- Args.get
   pkgDescr <- execute (command args) <$> readPackageDescription silent (Args.cabalFile args)
   putStrLn $ showGenericPackageDescription pkgDescr
