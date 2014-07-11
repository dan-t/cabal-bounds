{-# Language StandaloneDeriving, PatternGuards #-}

module CabalBounds.Main
   ( cabalBounds
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Simple.Configure (ConfigStateFileErrorType(..), tryGetConfigStateFile)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import qualified Distribution.Simple.LocalBuildInfo as BI
import qualified Distribution.Package as P
import qualified Distribution.Simple.PackageIndex as PX
import qualified Distribution.InstalledPackageInfo as PI
import qualified Distribution.Version as V
import qualified CabalBounds.Args as A
import qualified CabalBounds.Bound as B
import qualified CabalBounds.Sections as S
import qualified CabalBounds.Dependencies as DP
import qualified CabalBounds.Drop as D
import qualified CabalBounds.Update as U
import qualified CabalBounds.HaskellPlatform as HP
import qualified System.IO.Strict as SIO
import Control.Applicative ((<$>))
import Control.Monad.Trans.Either (EitherT, runEitherT, bimapEitherT, hoistEither, left, right)
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM

type Error = String

cabalBounds :: A.Args -> IO (Maybe Error)
cabalBounds args@A.Drop {} =
   leftToJust <$> runEitherT (do
      pkgDescrp <- packageDescription $ A.cabalFile args
      let pkgDescrp' = D.drop (B.boundOfDrop args) (S.sections args pkgDescrp) (DP.dependencies args) pkgDescrp
      liftIO $ writeFile (A.outputFile args) (showGenericPackageDescription pkgDescrp'))

cabalBounds args@A.Update {} =
   leftToJust <$> runEitherT (do
      pkgDescrp <- packageDescription $ A.cabalFile args
      libs      <- libraries setupConfigFile (A.haskellPlatform args)
      let pkgDescrp' = U.update (B.boundOfUpdate args) (S.sections args pkgDescrp) (DP.dependencies args) libs pkgDescrp
      liftIO $ writeFile (A.outputFile args) (showGenericPackageDescription pkgDescrp'))
   where
      setupConfigFile
         | (file:_) <- A.setupConfigFile args
         = file

         | otherwise
         = ""


packageDescription :: FilePath -> EitherT Error IO GenericPackageDescription
packageDescription file = do
   contents <- liftIO $ SIO.readFile file
   case parsePackageDescription contents of
        ParseFailed error   -> left $ show error
        ParseOk _ pkgDescrp -> right pkgDescrp


type SetupConfigFile = String

libraries :: SetupConfigFile -> HP.HPVersion -> EitherT Error IO U.Libraries
libraries "" ""              = left "Missing setup config file and haskell platform version"
libraries confFile ""        = installedLibraries confFile
libraries "" hpVersion       = haskellPlatformLibraries hpVersion
libraries confFile hpVersion = do
   instLibs <- installedLibraries confFile
   hpLibs   <- haskellPlatformLibraries hpVersion
   right $ HM.union hpLibs instLibs


haskellPlatformLibraries :: HP.HPVersion -> EitherT Error IO U.Libraries
haskellPlatformLibraries hpVersion =
   case hpVersion of
        "current"  -> right . HM.fromList $ HP.currentLibraries
        "previous" -> right . HM.fromList $ HP.previousLibraries
        version | Just libs <- HP.librariesOf version -> right . HM.fromList $ libs
                | otherwise                           -> left $ "Invalid haskell platform version '" ++ version ++ "'"


installedLibraries :: SetupConfigFile -> EitherT Error IO U.Libraries
installedLibraries confFile = do
   binfo <- liftIO $ tryGetConfigStateFile confFile
   bimapEitherT show buildInfoLibs (hoistEither binfo)
   where
      buildInfoLibs :: LocalBuildInfo -> U.Libraries
      buildInfoLibs = HM.fromList
                    . map (\(P.PackageName n, v) -> (n, newestVersion v))
                    . filter ((not . null) . snd)
                    . PX.allPackagesByName . BI.installedPkgs

      newestVersion :: [PI.InstalledPackageInfo] -> V.Version
      newestVersion = maximum . map (P.pkgVersion . PI.sourcePackageId)


leftToJust :: Either a b -> Maybe a
leftToJust = either Just (const Nothing)


deriving instance Show ConfigStateFileErrorType
