{-# Language StandaloneDeriving #-}

module CabalBounds.Main
   ( cabalBounds
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Simple.Configure (ConfigStateFileErrorType(..), tryGetConfigStateFile)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import qualified CabalBounds.Args as A
import qualified CabalBounds.Bound as B
import qualified CabalBounds.Sections as S
import qualified CabalBounds.Dependencies as DP
import qualified CabalBounds.Drop as D
import qualified CabalBounds.Update as U
import qualified System.IO.Strict as SIO
import Control.Applicative ((<$>))
import Control.Monad.Trans.Either (EitherT, runEitherT, bimapEitherT, hoistEither, left, right)
import Control.Monad.IO.Class

type Error = String

cabalBounds :: A.Args -> IO (Maybe Error)
cabalBounds args@A.Drop {} = do
   leftToJust <$> (runEitherT $ do
      pkgDescrp <- packageDescription $ A.cabalFile args
      let pkgDescrp' = D.drop (B.boundOfDrop args) (S.sections args) (DP.dependencies args) pkgDescrp
      liftIO $ writeFile (A.outputFile args) (showGenericPackageDescription pkgDescrp')
      right ())

cabalBounds args@A.Update {} = do
   leftToJust <$> (runEitherT $ do
      pkgDescrp <- packageDescription $ A.cabalFile args
      buildInfo <- localBuildInfo $ A.setupConfigFile args
      let pkgDescrp' = U.update (B.boundOfUpdate args) (S.sections args) (DP.dependencies args) pkgDescrp buildInfo
      liftIO $ writeFile (A.outputFile args) (showGenericPackageDescription pkgDescrp')
      right ())


packageDescription :: FilePath -> EitherT Error IO GenericPackageDescription
packageDescription file = do
   contents <- liftIO $ SIO.readFile file
   case parsePackageDescription contents of
        ParseFailed error   -> left $ show error
        ParseOk _ pkgDescrp -> right pkgDescrp


localBuildInfo :: FilePath -> EitherT Error IO LocalBuildInfo
localBuildInfo file =  do
   binfo <- liftIO $ tryGetConfigStateFile file
   bimapEitherT show id (hoistEither binfo)


leftToJust :: Either a b -> Maybe a
leftToJust = either Just (const Nothing)


deriving instance Show ConfigStateFileErrorType
