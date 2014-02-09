
module Main where

import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import qualified CabalBounds.Args as Args
import CabalBounds.Command (command)
import CabalBounds.Execute (execute)
import qualified System.IO.Strict as SIO
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
   args <- Args.get
   file <- SIO.readFile (Args.cabalFile args)
   case parsePackageDescription file of
        ParseFailed error   -> hPutStrLn stderr ("cabal-bounds: " ++ show error) >> exitFailure
        ParseOk _ pkgDescrp -> do
           let pkgDescrp' = execute (command args) pkgDescrp
               outFile | null $ Args.outputCabalFile args = Args.cabalFile args
                       | otherwise                        = Args.outputCabalFile args

           writeFile outFile (showGenericPackageDescription pkgDescrp')
           exitSuccess
