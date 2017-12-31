
module Main where

import qualified Test.Tasty as T
import qualified Test.Tasty.Golden as G
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>), (<.>))
import System.FilePath.Glob (glob)
import qualified System.Directory as Dir
import qualified System.Process as Proc
import CabalBounds.Args
import CabalBounds.Main (cabalBounds)
import CabalBounds.VersionComp (VersionComp(..))
import Control.Exception (finally)


data LibsSource = SetupConfig | PlanFile
   deriving (Show, Eq)


main :: IO ()
main = do
   buildSource SetupConfig
   T.defaultMain tests


buildSource :: LibsSource -> IO ()
buildSource source = do
   curDir <- Dir.getCurrentDirectory
   build source curDir `finally` cleanUp source curDir
   where
      build SetupConfig curDir = do
         let inputFiles = curDir </> "tests" </> "inputFiles"
         let buildDir   = inputFiles </> "setup-config-build-env"
         let libsDir    = buildDir </> "libs"
         Dir.setCurrentDirectory buildDir

         Proc.runCommand "cabal sandbox init" >>= Proc.waitForProcess
         Proc.runCommand ("cabal sandbox add-source " ++ (libsDir </> "A")) >>= Proc.waitForProcess
         Proc.runCommand ("cabal sandbox add-source " ++ (libsDir </> "B")) >>= Proc.waitForProcess
         Proc.runCommand ("cabal sandbox add-source " ++ (libsDir </> "C")) >>= Proc.waitForProcess
         Proc.runCommand ("cabal sandbox add-source " ++ (libsDir </> "D")) >>= Proc.waitForProcess
         Proc.runCommand "cabal install" >>= Proc.waitForProcess

         [distDir] <- glob $ "dist" </> "dist-sandbox-*"
         Dir.copyFile (distDir </> "setup-config") (inputFiles </> "setup-config")

      build PlanFile _ = return ()


      cleanUp SetupConfig curDir = do
         let buildDir = curDir </> "tests" </> "inputFiles" </> "setup-config-build-env"
         Dir.setCurrentDirectory buildDir
         Proc.runCommand "cabal sandbox delete" >>= Proc.waitForProcess

         Proc.runCommand "cabal clean" >>= Proc.waitForProcess
         Dir.setCurrentDirectory curDir

      cleanUp PlanFile _ = return ()


tests :: T.TestTree
tests = T.testGroup "Tests" [ T.testGroup "Sourceless Tests" [dropTests, dumpTests]
                            , T.testGroup "SetupConfig Tests" [updateTests SetupConfig, libsTests SetupConfig]
                            , T.testGroup "PlanFile Tests" [updateTests PlanFile, libsTests PlanFile]
                            ]


dropTests :: T.TestTree
dropTests = T.testGroup "Drop Tests"
   [ test Nothing "DropBothOfAll" defaultDrop
   , test Nothing "DropUpperOfAll" $ defaultDrop { upper = True }
   , test Nothing "DropBothOfLib" $ defaultDrop { library = True }
   , test Nothing "DropUpperOfLib" $ defaultDrop { upper = True, library = True }
   , test Nothing "DropBothOfExe" $ defaultDrop { executable = ["cabal-bounds"] }
   , test Nothing "DropUpperOfExe" $ defaultDrop { upper = True, executable = ["cabal-bounds"] }
   , test Nothing "DropBothOfOtherExe" $ defaultDrop { executable = ["other-exe"] }
   , test Nothing "DropUpperOfOtherExe" $ defaultDrop { upper = True, executable = ["other-exe"] }
   , test Nothing "DropBothOfAllExes" $ defaultDrop { executable = ["cabal-bounds", "other-exe"] }
   , test Nothing "DropUpperOfAllExes" $ defaultDrop { upper = True, executable = ["cabal-bounds", "other-exe"] }
   , test Nothing "DropBothOfTest" $ defaultDrop { testSuite = ["some-test"] }
   , test Nothing "DropUpperOfTest" $ defaultDrop { upper = True, testSuite = ["some-test"] }
   , test Nothing "DropBothOnlyBase" $ defaultDrop { only = ["base"] }
   , test Nothing "DropUpperOnlyBase" $ defaultDrop { upper = True, only = ["base"] }
   , test Nothing "DropBothIgnoreA" $ defaultDrop { ignore = ["A"] }
   , test Nothing "DropUpperIgnoreA" $ defaultDrop { upper = True, ignore = ["A"] }
   ]


updateTests :: LibsSource -> T.TestTree
updateTests source = T.testGroup "Update Tests"
   [ test (Just source) "UpdateBothOfAll" defaultUpdate
   , test (Just source) "UpdateBothOfAll" $ defaultUpdate { lower = True, upper = True }
   , test (Just source) "UpdateBothOfAllExes" $ defaultUpdate { executable = ["cabal-bounds", "other-exe"] }
   , test (Just source) "UpdateBothOfExe" $ defaultUpdate { executable = ["cabal-bounds"] }
   , test (Just source) "UpdateBothOfLibrary" $ defaultUpdate { library = True }
   , test (Just source) "UpdateBothOfOtherExe" $ defaultUpdate { executable = ["other-exe"] }
   , test (Just source) "UpdateBothOfTest" $ defaultUpdate { testSuite = ["some-test"] }
   , test (Just source) "UpdateLowerOfAll" $ defaultUpdate { lower = True }
   , test (Just source) "UpdateLowerOfAllExes" $ defaultUpdate { lower = True, executable = ["cabal-bounds", "other-exe"] }
   , test (Just source) "UpdateLowerOfExe" $ defaultUpdate { lower = True, executable = ["cabal-bounds"] }
   , test (Just source) "UpdateLowerOfLibrary" $ defaultUpdate { lower = True, library = True }
   , test (Just source) "UpdateLowerOfOtherExe" $ defaultUpdate { lower = True, executable = ["other-exe"] }
   , test (Just source) "UpdateLowerOfTest" $ defaultUpdate { lower = True, testSuite = ["some-test"] }
   , test (Just source) "UpdateUpperOfAll" $ defaultUpdate { upper = True }
   , test (Just source) "UpdateUpperOfAllExes" $ defaultUpdate { upper = True, executable = ["cabal-bounds", "other-exe"] }
   , test (Just source) "UpdateUpperOfExe" $ defaultUpdate { upper = True, executable = ["cabal-bounds"] }
   , test (Just source) "UpdateUpperOfLibrary" $ defaultUpdate { upper = True, library = True }
   , test (Just source) "UpdateUpperOfOtherExe" $ defaultUpdate { upper = True, executable = ["other-exe"] }
   , test (Just source) "UpdateUpperOfTest" $ defaultUpdate { upper = True, testSuite = ["some-test"] }
   , test (Just source) "UpdateBothIgnoreA" $ defaultUpdate { ignore = ["A"] }
   , test (Just source) "UpdateMinorLower" $ defaultUpdate { lowerComp = Just Minor }
   , test (Just source) "UpdateMajor2Lower" $ defaultUpdate { lowerComp = Just Major2 }
   , test (Just source) "UpdateMajor1Lower" $ defaultUpdate { lowerComp = Just Major1 }
   , test (Just source) "UpdateMinorUpper" $ defaultUpdate { upperComp = Just Minor }
   , test (Just source) "UpdateMajor2Upper" $ defaultUpdate { upperComp = Just Major2 }
   , test (Just source) "UpdateMajor1Upper" $ defaultUpdate { upperComp = Just Major1 }
   , test (Just source) "UpdateMinorLowerAndUpper" $ defaultUpdate { lowerComp = Just Minor, upperComp = Just Minor }
   , test (Just source) "UpdateMajor1LowerAndUpper" $ defaultUpdate { lowerComp = Just Major1, upperComp = Just Major1 }
   , test (Just source) "UpdateOnlyMissing" $ defaultUpdate { missing = True }
   , test (Just source) "UpdateByHaskellPlatform" $ defaultUpdate { haskellPlatform = "2013.2.0.0" }
   , test (Just source) "UpdateUpperFromFile" $ defaultUpdate { upper = True, fromFile = "tests" </> "inputFiles" </> "FromFile.hs" }
   ]


dumpTests :: T.TestTree
dumpTests = T.testGroup "Dump Tests"
   [ test Nothing "Dump" defaultDump
   ]


libsTests :: LibsSource -> T.TestTree
libsTests source = T.testGroup "Libs Tests"
   [ test (Just source) "Libs" defaultLibs
   ]


test :: Maybe LibsSource -> String -> Args -> T.TestTree
test source testName args =
   G.goldenVsFileDiff testName diff goldenFile outputFile command
   where
      command = do
         error <- cabalBounds argsWithFiles
         case error of
              Just err -> hPutStrLn stderr ("cabal-bounds: " ++ err)
              _        -> return ()

      argsWithFiles =
         case args of
              Drop {}   -> args { cabalFile = Just inputFile
                                , output    = Just outputFile
                                , ignore    = ["base"] ++ ignore args
                                }
              Update {} -> args { cabalFile       = Just inputFile
                                , output          = Just outputFile
                                , setupConfigFile = setupConfigFile
                                , planFile        = planFile
                                , ignore          = ["base"] ++ ignore args
                                }

              Dump {}   -> args { cabalFiles = [inputFile]
                                , output     = Just outputFile
                                , ignore     = ["base"] ++ ignore args
                                }

              Libs {}   -> args { cabalFile       = Just inputFile
                                , output          = Just outputFile
                                , setupConfigFile = setupConfigFile
                                , planFile        = planFile
                                , ignore          = ["base", "ghc-prim", "integer-gmp", "rts"] ++ ignore args
                                }

      diff ref new    = ["diff", "-u", ref, new]

      goldenFile      = "tests" </> "goldenFiles" </> testName <.> (if hasHsOutput then "hs" else "cabal")

      outputFile      =
         case source of
              Nothing  -> "tests" </> "outputFiles" </> testName <.> (if hasHsOutput then "hs" else "cabal")
              Just src -> "tests" </> "outputFiles" </> (show src) </> testName <.> (if hasHsOutput then "hs" else "cabal")

      inputFile       = "tests" </> "inputFiles" </> (inputFileName testName)
         where
            inputFileName "UpdateByHaskellPlatform" = "hp-original.cabal"
            inputFileName "UpdateOnlyMissing"       = "missing-original.cabal"
            inputFileName _                         = "original.cabal"

      setupConfigFile =
         case source of
              Just SetupConfig -> Just $ "tests" </> "inputFiles" </> "setup-config"
              _                -> Nothing

      planFile =
         case source of
              Just PlanFile -> Just $ "tests" </> "inputFiles" </> "plan.json"
              _             -> Nothing

      hasHsOutput =
         case args of
              Dump {} -> True
              Libs {} -> True
              _       -> False
