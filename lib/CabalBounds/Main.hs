{-# Language StandaloneDeriving, PatternGuards, CPP #-}

module CabalBounds.Main
   ( cabalBounds
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Simple.Configure (tryGetConfigStateFile)

#if MIN_VERSION_Cabal(1,22,0) == 0
import Distribution.Simple.Configure (ConfigStateFileErrorType(..))
#endif

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
import qualified CabalBounds.Dump as D
import qualified CabalBounds.HaskellPlatform as HP
import qualified CabalLenses as CL
import qualified System.IO.Strict as SIO
import Control.Applicative ((<$>))
import Control.Monad.Trans.Either (EitherT, runEitherT, bimapEitherT, hoistEither, left, right)
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', sortBy)
import Data.Function (on)
import Data.Char (toLower)

type Error = String

cabalBounds :: A.Args -> IO (Maybe Error)
cabalBounds args@A.Drop {} =
   leftToJust <$> runEitherT (do
      pkgDescrp <- packageDescription $ A.cabalFile args
      let pkgDescrp' = D.drop (B.boundOfDrop args) (S.sections args pkgDescrp) (DP.dependencies args) pkgDescrp
      liftIO $ writeFile (A.outputFile args) (showGenericPackageDescription . clearTargetBuildDepends $ pkgDescrp'))

cabalBounds args@A.Update {} =
   leftToJust <$> runEitherT (do
      pkgDescrp <- packageDescription $ A.cabalFile args
      libs      <- libraries (A.haskellPlatform args) (A.fromFile args) setupConfigFile
      let pkgDescrp' = U.update (B.boundOfUpdate args) (S.sections args pkgDescrp) (DP.dependencies args) libs pkgDescrp
      liftIO $ writeFile (A.outputFile args) (showGenericPackageDescription . clearTargetBuildDepends $ pkgDescrp'))
   where
      setupConfigFile
         | (file:_) <- A.setupConfigFile args
         = file

         | otherwise
         = ""

cabalBounds args@A.Dump {} =
   leftToJust <$> runEitherT (do
      pkgDescrps <- packageDescriptions $ A.cabalFiles args
      let libs = sortBy (compare `on` (map toLower . fst)) $ D.dump pkgDescrps
      if (not . null . A.outputFile $ args)
         then liftIO $ writeFile (A.outputFile args) (prettyPrint libs)
         else liftIO $ putStrLn (prettyPrint libs))
   where
      prettyPrint []     = "[]"
      prettyPrint (l:ls) =
         "[ " ++ show l ++ "\n" ++ foldl' (\str l -> str ++ ", " ++ show l ++ "\n") "" ls ++ "]";


packageDescription :: FilePath -> EitherT Error IO GenericPackageDescription
packageDescription file = do
   contents <- liftIO $ SIO.readFile file
   case parsePackageDescription contents of
        ParseFailed error   -> left $ show error
        ParseOk _ pkgDescrp -> right pkgDescrp


-- | clear the 'targetBuildDepends' field of all 'BuildInfo'
clearTargetBuildDepends :: GenericPackageDescription -> GenericPackageDescription
clearTargetBuildDepends pkgDescrp =
   pkgDescrp & CL.allBuildInfo . CL.targetBuildDependsL .~ []


packageDescriptions :: [FilePath] -> EitherT Error IO [GenericPackageDescription]
packageDescriptions []    = left "Missing cabal file"
packageDescriptions files = mapM packageDescription files


type SetupConfigFile = String
type LibraryFile     = String

libraries :: HP.HPVersion -> LibraryFile -> SetupConfigFile -> EitherT Error IO U.Libraries
libraries "" "" ""                   = left "Missing library file, haskell platform version and setup config file"
libraries hpVersion libFile confFile = do
   hpLibs       <- haskellPlatformLibraries hpVersion
   libsFromFile <- librariesFromFile libFile
   instLibs     <- installedLibraries confFile
   right $ HM.union (HM.union hpLibs libsFromFile) instLibs


librariesFromFile :: LibraryFile -> EitherT Error IO U.Libraries
librariesFromFile ""      = right HM.empty
librariesFromFile libFile = do
   contents <- liftIO $ SIO.readFile libFile
   libsFrom contents
   where
      libsFrom contents
         | [(libs, _)] <- reads contents :: [([(String, [Int])], String)]
         = right $ HM.fromList (map (\(pkgName, versBranch) -> (pkgName, V.Version versBranch [])) libs)

         | otherwise
         = left "Invalid format of library file given to '--fromfile'. Expected file with content of type '[(String, [Int])]'."


haskellPlatformLibraries :: HP.HPVersion -> EitherT Error IO U.Libraries
haskellPlatformLibraries hpVersion =
   case hpVersion of
        ""         -> right HM.empty
        "current"  -> right . HM.fromList $ HP.currentLibraries
        "previous" -> right . HM.fromList $ HP.previousLibraries
        version | Just libs <- HP.librariesOf version -> right . HM.fromList $ libs
                | otherwise                           -> left $ "Invalid haskell platform version '" ++ version ++ "'"


installedLibraries :: SetupConfigFile -> EitherT Error IO U.Libraries
installedLibraries ""       = right HM.empty
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

#if MIN_VERSION_Cabal(1,22,0) == 0
deriving instance Show ConfigStateFileErrorType
#endif
