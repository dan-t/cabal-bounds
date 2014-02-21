
module CabalBounds.Main
   ( cabalBounds
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Simple.Configure (tryGetConfigStateFile)
import qualified CabalBounds.Args as A
import qualified CabalBounds.Bound as B
import qualified CabalBounds.Targets as T
import qualified CabalBounds.Drop as D
import qualified CabalBounds.Update as U
import qualified System.IO.Strict as SIO

type Error = String

cabalBounds :: A.Args -> IO (Maybe Error)
cabalBounds args@A.Drop {} = do
   pkgDescrp <- packageDescription $ A.cabalFile args
   case pkgDescrp of
        Left  error      -> return . Just $ error
        Right pkgDescrp_ -> do
           let pkgDescrp' = D.drop (B.bound args) (T.targets args) pkgDescrp_
           writeFile (A.outputFile args) (showGenericPackageDescription pkgDescrp')
           return Nothing

cabalBounds args@A.Update {} = do
   pkgDescrp <- packageDescription $ A.cabalFile args
   buildInfo <- tryGetConfigStateFile $ A.setupConfigFile args
   case (pkgDescrp, buildInfo) of
        (Left error, _) -> return . Just $ error
        (_, Left error) -> return . Just $ error
        (Right pkgDescrp_, Right buildInfo_) -> do
           let pkgDescrp' = U.update (B.bound args) (T.targets args) pkgDescrp_ buildInfo_
           writeFile (A.outputFile args) (showGenericPackageDescription pkgDescrp')
           return Nothing


packageDescription :: FilePath -> IO (Either Error GenericPackageDescription)
packageDescription file = do
   contents <- SIO.readFile file
   case parsePackageDescription contents of
        ParseFailed error   -> return . Left  $ show error
        ParseOk _ pkgDescrp -> return . Right $ pkgDescrp
