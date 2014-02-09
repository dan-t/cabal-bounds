
module CabalBounds.Main
   ( cabalBounds
   ) where

import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import qualified CabalBounds.Args as Args
import CabalBounds.Command (command)
import CabalBounds.Execute (execute)
import qualified System.IO.Strict as SIO

type Error = String

cabalBounds :: Args.Args -> IO (Maybe Error)
cabalBounds args  = do
   file <- SIO.readFile (Args.cabalFile args)
   case parsePackageDescription file of
        ParseFailed error   -> return . Just $ show error
        ParseOk _ pkgDescrp -> do
           let pkgDescrp' = execute (command args) pkgDescrp
               outFile | null $ Args.outputCabalFile args = Args.cabalFile args
                       | otherwise                        = Args.outputCabalFile args

           writeFile outFile (showGenericPackageDescription pkgDescrp')
           return Nothing
