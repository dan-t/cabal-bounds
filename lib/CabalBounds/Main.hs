{-# Language StandaloneDeriving, PatternGuards, CPP, OverloadedStrings #-}

module CabalBounds.Main
   ( cabalBounds
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import qualified Distribution.PackageDescription.PrettyPrint as PP
import Distribution.Simple.Configure (tryGetConfigStateFile)
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
import CabalBounds.Types
import qualified CabalLenses as CL
import qualified System.IO.Strict as SIO
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Control.Monad.Trans.Either (EitherT, runEitherT, bimapEitherT, hoistEither, left, right)
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', sortBy, find)
import Data.Function (on)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T

#if MIN_VERSION_Cabal(1,22,0) == 0
import Distribution.Simple.Configure (ConfigStateFileErrorType(..))
#endif

#if MIN_VERSION_Cabal(1,22,0) && MIN_VERSION_Cabal(1,22,1) == 0
import Control.Lens
#endif

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif


type Error = String
type SetupConfigFile = FilePath
type PlanFile = FilePath
type LibraryFile = FilePath
type CabalFile = FilePath


cabalBounds :: A.Args -> IO (Maybe Error)
cabalBounds args'@A.Drop {} =
   leftToJust <$> runEitherT (do
      cabalFile <- findCabalFile $ A.cabalFile args
      pkgDescrp <- packageDescription cabalFile
      let pkgDescrp' = D.drop (B.boundOfDrop args) (S.sections args pkgDescrp) (DP.dependencies args) pkgDescrp
      let outputFile = fromMaybe cabalFile (A.output args)
      liftIO $ writeFile outputFile (showGenericPackageDescription pkgDescrp'))
   where
      args = ignoreBaseLibrary args'

cabalBounds args'@A.Update {} =
   leftToJust <$> runEitherT (do
      cabalFile <- findCabalFile $ A.cabalFile args
      pkgDescrp <- packageDescription cabalFile
      let haskelPlatform = A.haskellPlatform args
          libFile        = A.fromFile args
          configFile     = A.setupConfigFile args
          planFile       = A.planFile args
      libs      <- libraries haskelPlatform libFile configFile planFile cabalFile
      let pkgDescrp' = U.update (B.boundOfUpdate args) (S.sections args pkgDescrp) (DP.dependencies args) libs pkgDescrp
      let outputFile = fromMaybe cabalFile (A.output args)
      liftIO $ writeFile outputFile (showGenericPackageDescription pkgDescrp'))
   where
      args = ignoreBaseLibrary args'

cabalBounds args'@A.Dump {} =
   leftToJust <$> runEitherT (do
      cabalFiles <- if null $ A.cabalFiles args
                       then (: []) <$> findCabalFile Nothing
                       else right $ A.cabalFiles args

      pkgDescrps <- packageDescriptions cabalFiles
      let libs = sortLibraries $ D.dump (DP.dependencies args) pkgDescrps
      case A.output args of
           Just file -> liftIO $ writeFile file (prettyPrint libs)
           Nothing   -> liftIO $ putStrLn (prettyPrint libs))
   where
      args = ignoreBaseLibrary args'

cabalBounds args@A.Libs {} =
   leftToJust <$> runEitherT (do
      cabalFile <- findCabalFile $ A.cabalFile args
      let haskelPlatform = A.haskellPlatform args
          libFile        = A.fromFile args
          configFile     = A.setupConfigFile args
          planFile       = A.planFile args
      libs <- sortLibraries . toList <$> libraries haskelPlatform libFile configFile planFile cabalFile
      let libs' = libs ^.. traversed . DP.filterLibrary (DP.dependencies args)
      case A.output args of
           Just file -> liftIO $ writeFile file (prettyPrint libs')
           Nothing   -> liftIO $ putStrLn (prettyPrint libs'))


sortLibraries :: Libraries -> Libraries
sortLibraries = sortBy (compare `on` (map toLower . fst))


prettyPrint :: Libraries -> String
prettyPrint []     = "[]"
prettyPrint (l:ls) =
   "[ " ++ show l ++ "\n" ++ foldl' (\str l -> str ++ ", " ++ show l ++ "\n") "" ls ++ "]\n";


findCabalFile :: Maybe CabalFile -> EitherT Error IO CabalFile
findCabalFile Nothing = do
   curDir <- liftIO getCurrentDirectory
   CL.findCabalFile curDir

findCabalFile (Just file) = right file


ignoreBaseLibrary :: A.Args -> A.Args
ignoreBaseLibrary args =
   case find (== "base") (A.ignore args) of
        Just _  -> args
        Nothing -> args { A.ignore = "base" : A.ignore args }


packageDescription :: FilePath -> EitherT Error IO GenericPackageDescription
packageDescription file = do
   contents <- liftIO $ SIO.readFile file
   case parsePackageDescription contents of
        ParseFailed error   -> left $ show error
        ParseOk _ pkgDescrp -> right pkgDescrp


packageDescriptions :: [FilePath] -> EitherT Error IO [GenericPackageDescription]
packageDescriptions []    = left "Missing cabal file"
packageDescriptions files = mapM packageDescription files


libraries :: HP.HPVersion -> LibraryFile -> Maybe SetupConfigFile -> Maybe PlanFile -> CabalFile -> EitherT Error IO LibraryMap
libraries "" "" (Just confFile) _ _ = do
   librariesFromSetupConfig confFile

libraries "" "" _ (Just planFile) _ = do
   librariesFromPlanFile planFile

libraries "" "" Nothing Nothing cabalFile = do
   distDir <- liftIO $ CL.findDistDir cabalFile
   case distDir of
        Just distDir -> librariesFromSetupConfig $ distDir </> "setup-config"
        Nothing      -> do
           newDistDir <- liftIO $ CL.findNewDistDir cabalFile
           case newDistDir of
                Just newDistDir -> librariesFromPlanFile $ newDistDir </> "cache" </> "plan.json"
                Nothing         -> left "Couldn't find 'dist' nor 'dist-newstyle' directory! Have you already build the cabal project?"

libraries hpVersion libFile _ _ _ = do
   hpLibs       <- haskellPlatformLibraries hpVersion
   libsFromFile <- librariesFromFile libFile
   right $ HM.union hpLibs libsFromFile


librariesFromFile :: LibraryFile -> EitherT Error IO LibraryMap
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


haskellPlatformLibraries :: HP.HPVersion -> EitherT Error IO LibraryMap
haskellPlatformLibraries hpVersion =
   case hpVersion of
        ""         -> right HM.empty
        "current"  -> right . HM.fromList $ HP.currentLibraries
        "previous" -> right . HM.fromList $ HP.previousLibraries
        version | Just libs <- HP.librariesOf version -> right . HM.fromList $ libs
                | otherwise                           -> left $ "Invalid haskell platform version '" ++ version ++ "'"


librariesFromSetupConfig :: SetupConfigFile -> EitherT Error IO LibraryMap
librariesFromSetupConfig ""       = right HM.empty
librariesFromSetupConfig confFile = do
   binfo <- liftIO $ tryGetConfigStateFile confFile
   bimapEitherT show buildInfoLibs (hoistEither binfo)
   where
      buildInfoLibs :: LocalBuildInfo -> LibraryMap
      buildInfoLibs = HM.fromList
                    . map (\(P.PackageName n, v) -> (n, newestVersion v))
                    . filter ((not . null) . snd)
                    . PX.allPackagesByName . BI.installedPkgs

      newestVersion :: [PI.InstalledPackageInfo] -> V.Version
      newestVersion = maximum . map (P.pkgVersion . PI.sourcePackageId)


librariesFromPlanFile :: PlanFile -> EitherT Error IO LibraryMap
librariesFromPlanFile planFile = do
   contents <- liftIO $ BS.readFile planFile
   let json = Aeson.decode contents :: Maybe Aeson.Value
   case json of
        Just json -> do
           -- get all ids: ["bytestring-0.10.6.0-2362d1f36f12553920ce3710ae4a4ecb432374f4e5feb33a61b7414b43605a0df", ...]
           let ids = json ^.. key "install-plan" . _Array . traversed . key "id" . _String

           -- transform ids into: [["2362d1f36f12553920ce3710ae4a4ecb432374f4e5feb33a61b7414b43605a0df", "0.10.6.0", "bytestring"], ...]
           let ids' = map (reverse . T.split (== '-')) ids

           -- remove too short ids or ids of inplace libs
           let ids'' = filter (\id -> length id >= 3 && (id !! 0) /= "inplace") ids'

           -- drop the hash: [["0.10.6.0", "bytestring"], ...]
           let ids''' = map (drop 1) ids''

           -- transform verions into: [[0, 10, 6, 0], ...]
           let versions = map (T.split (== '.') . head) ids'''
           let versions' = map (map (\s -> read (T.unpack s) :: Int)) versions
           let versions'' = map (\v -> V.Version { V.versionBranch = v, V.versionTags = [] }) versions'

           let names = map (reverse . tail) ids'''
           let names' = map (T.intercalate "-") names
           let names'' = map T.unpack names'

           right . HM.fromList $ zip names'' versions''

        Nothing   -> left $ "Couldn't parse json file '" ++ planFile ++ "'"


leftToJust :: Either a b -> Maybe a
leftToJust = either Just (const Nothing)


showGenericPackageDescription :: GenericPackageDescription -> String
showGenericPackageDescription =
#if MIN_VERSION_Cabal(1,22,1)
   PP.showGenericPackageDescription
#elif MIN_VERSION_Cabal(1,22,0)
   PP.showGenericPackageDescription . clearTargetBuildDepends
   where
      clearTargetBuildDepends pkgDescrp =
         pkgDescrp & CL.allBuildInfo . CL.targetBuildDependsL .~ []
#else
   ensureLastIsNewline . PP.showGenericPackageDescription
   where
      ensureLastIsNewline xs =
         if last xs == '\n' then xs else xs ++ "\n"
#endif


#if MIN_VERSION_Cabal(1,22,0) == 0
deriving instance Show ConfigStateFileErrorType
#endif
