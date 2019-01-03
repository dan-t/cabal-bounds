{-# Language StandaloneDeriving, PatternGuards, CPP, OverloadedStrings #-}

module CabalBounds.Main
   ( cabalBounds
   ) where

import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec.Common (PWarning)
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
import qualified CabalBounds.Drop as DR
import qualified CabalBounds.Update as U
import qualified CabalBounds.Dump as DU
import qualified CabalBounds.HaskellPlatform as HP
import CabalBounds.Types
import qualified CabalLenses as CL
import qualified System.IO.Strict as SIO
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', sortBy)
import Data.Function (on)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.List as L
import Text.Read (readMaybe)


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
cabalBounds args@A.Drop {} =
   leftToJust <$> runExceptT (do
      cabalFile <- findCabalFile $ A.cabalFile args
      pkgDescrp <- packageDescription cabalFile
      let pkgDescrp' = DR.drop (B.boundOfDrop args) (S.sections args pkgDescrp) (DP.dependencies args) pkgDescrp
      let outputFile = fromMaybe cabalFile (A.output args)
      liftIO $ writeFile outputFile (showGenericPackageDescription pkgDescrp'))

cabalBounds args@A.Update {} =
   leftToJust <$> runExceptT (do
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

cabalBounds args@A.Dump {} =
   leftToJust <$> runExceptT (do
      cabalFiles <- if null $ A.cabalFiles args
                       then (: []) <$> findCabalFile Nothing
                       else return $ A.cabalFiles args

      pkgDescrps <- packageDescriptions cabalFiles
      let libs = sortLibraries $ DU.dump (DP.dependencies args) pkgDescrps
      case A.output args of
           Just file -> liftIO $ writeFile file (prettyPrint libs)
           Nothing   -> liftIO $ putStrLn (prettyPrint libs))

cabalBounds args@A.Libs {} =
   leftToJust <$> runExceptT (do
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


cabalBounds args@A.Format {} =
   leftToJust <$> runExceptT (do
      cabalFile <- findCabalFile $ A.cabalFile args
      pkgDescrp <- packageDescription cabalFile
      let outputFile = fromMaybe cabalFile (A.output args)
      liftIO $ writeFile outputFile (showGenericPackageDescription pkgDescrp))


sortLibraries :: Libraries -> Libraries
sortLibraries = sortBy (compare `on` (map toLower . fst))


prettyPrint :: Libraries -> String
prettyPrint []     = "[]"
prettyPrint (l:ls) =
   "[ " ++ show l ++ "\n" ++ foldl' (\str l -> str ++ ", " ++ show l ++ "\n") "" ls ++ "]\n";


findCabalFile :: Maybe CabalFile -> ExceptT Error IO CabalFile
findCabalFile Nothing = do
   curDir <- liftIO getCurrentDirectory
   CL.findCabalFile curDir

findCabalFile (Just file) = return file


packageDescription :: FilePath -> ExceptT Error IO GenericPackageDescription
packageDescription file = do
   contents <- liftIO $ BS.readFile file
   let (warnings, result) = runParseResult $ parseGenericPackageDescription contents
   liftIO $ showWarnings warnings
   case result of
        Left (_, errors) -> throwE $ show errors
        Right pkgDescrp  -> return pkgDescrp

   where
      showWarnings :: [PWarning] -> IO ()
      showWarnings [] = return ()
      showWarnings ws = putStrLn $ "cabal-bounds: " ++ (L.intercalate ", " $ map show ws)


packageDescriptions :: [FilePath] -> ExceptT Error IO [GenericPackageDescription]
packageDescriptions []    = throwE "Missing cabal file"
packageDescriptions files = mapM packageDescription files


libraries :: HP.HPVersion -> LibraryFile -> Maybe SetupConfigFile -> Maybe PlanFile -> CabalFile -> ExceptT Error IO LibraryMap
libraries "" "" (Just confFile) _ _ = do
   librariesFromSetupConfig confFile

libraries "" "" _ (Just planFile) _ = do
   librariesFromPlanFile planFile

libraries "" "" Nothing Nothing cabalFile = do
   newDistDir <- liftIO $ CL.findNewDistDir cabalFile
   case newDistDir of
        Just newDistDir -> librariesFromPlanFile $ newDistDir </> "cache" </> "plan.json"
        Nothing         -> do
           distDir <- liftIO $ CL.findDistDir cabalFile
           case distDir of
                Just distDir -> librariesFromSetupConfig $ distDir </> "setup-config"
                Nothing      -> throwE "Couldn't find 'dist-newstyle' nor 'dist' directory! Have you already build the cabal project?"

libraries hpVersion libFile _ _ _ = do
   hpLibs       <- haskellPlatformLibraries hpVersion
   libsFromFile <- librariesFromFile libFile
   return $ HM.union hpLibs libsFromFile


librariesFromFile :: LibraryFile -> ExceptT Error IO LibraryMap
librariesFromFile ""      = return HM.empty
librariesFromFile libFile = do
   contents <- liftIO $ SIO.readFile libFile
   libsFrom contents
   where
      libsFrom contents
         | [(libs, _)] <- reads contents :: [([(String, [Int])], String)]
         = return $ HM.fromList (map (\(pkgName, versBranch) -> (pkgName, V.mkVersion versBranch)) libs)

         | otherwise
         = throwE "Invalid format of library file given to '--fromfile'. Expected file with content of type '[(String, [Int])]'."


haskellPlatformLibraries :: HP.HPVersion -> ExceptT Error IO LibraryMap
haskellPlatformLibraries hpVersion =
   case hpVersion of
        ""         -> return HM.empty
        "current"  -> return . HM.fromList $ HP.currentLibraries
        "previous" -> return . HM.fromList $ HP.previousLibraries
        version | Just libs <- HP.librariesOf version -> return . HM.fromList $ libs
                | otherwise                           -> throwE $ "Invalid haskell platform version '" ++ version ++ "'"


librariesFromSetupConfig :: SetupConfigFile -> ExceptT Error IO LibraryMap
librariesFromSetupConfig ""       = return HM.empty
librariesFromSetupConfig confFile = do
   binfo <- liftIO $ tryGetConfigStateFile confFile
   case binfo of
        Left e   -> throwE $ show e
        Right bi -> return $ buildInfoLibs bi
   where
      buildInfoLibs :: LocalBuildInfo -> LibraryMap
      buildInfoLibs = HM.fromList
                    . map (\(pkg, v) -> (P.unPackageName pkg, newestVersion v))
                    . filter ((not . null) . snd)
                    . PX.allPackagesByName . BI.installedPkgs

      newestVersion :: [PI.InstalledPackageInfo] -> V.Version
      newestVersion = maximum . map (P.pkgVersion . PI.sourcePackageId)


librariesFromPlanFile :: PlanFile -> ExceptT Error IO LibraryMap
librariesFromPlanFile planFile = do
   contents <- liftIO $ LBS.readFile planFile
   let json = Aeson.decode contents :: Maybe Aeson.Value
   case json of
        Just json -> do
           -- get all ids: ["bytestring-0.10.6.0-2362d1f36f12553920ce3710ae4a4ecb432374f4e5feb33a61b7414b43605a0df", ...]
           let ids = json ^.. key "install-plan" . _Array . traversed . key "id" . _String
           let libs = catMaybes $ map parseLibrary ids
           return . HM.fromList $ libs

        Nothing   -> throwE $ "Couldn't parse json file '" ++ planFile ++ "'"
   where
      parseLibrary :: Text -> Maybe (LibName, V.Version)
      parseLibrary text =
         case T.breakOnEnd "-" text of
              (_, "")         -> Nothing
              (_, "inplace")  -> Nothing
              (before, after) ->
                 case parseVersion after of
                      Just vers -> Just (T.unpack . stripSuffix "-" $ before, vers)
                      _         -> parseLibrary $ stripSuffix "-" before

      parseVersion :: Text -> Maybe V.Version
      parseVersion text =
         case catMaybes $ map (readMaybe . T.unpack) $ T.split (== '.') text of
              []   -> Nothing
              nums -> Just $ V.mkVersion nums

      stripSuffix :: Text -> Text -> Text
      stripSuffix suffix text = fromMaybe text (T.stripSuffix suffix text)


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
