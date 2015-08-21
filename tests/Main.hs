
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
import Control.Monad (when)
import Control.Exception (finally)

main :: IO ()
main = ensureSetupConfig >> T.defaultMain tests


ensureSetupConfig :: IO ()
ensureSetupConfig = do
   configExists <- Dir.doesFileExist $ "tests" </> "inputFiles" </> "setup-config"
   when (not configExists) $ do
      curDir <- Dir.getCurrentDirectory
      buildSetupConfig curDir `finally` cleanUp curDir
   where
      buildSetupConfig curDir = do
         let inputFiles = curDir </> "tests" </> "inputFiles"
         let buildDir   = inputFiles </> "setup-config-build-env"
         let libsDir    = buildDir </> "libs"
         Dir.setCurrentDirectory buildDir

         Proc.runCommand "cabal sandbox init" >>= Proc.waitForProcess
         Proc.runCommand ("cabal sandbox add-source " ++ (libsDir </> "A")) >>= Proc.waitForProcess
         Proc.runCommand ("cabal sandbox add-source " ++ (libsDir </> "B")) >>= Proc.waitForProcess
         Proc.runCommand ("cabal sandbox add-source " ++ (libsDir </> "C")) >>= Proc.waitForProcess
         Proc.runCommand "cabal install" >>= Proc.waitForProcess

         [distDir] <- glob $ "dist" </> "dist-sandbox-*"
         Dir.copyFile (distDir </> "setup-config") (inputFiles </> "setup-config")

      cleanUp prevDir = do
         let buildDir = prevDir </> "tests" </> "inputFiles" </> "setup-config-build-env"
         Dir.setCurrentDirectory buildDir
         Proc.runCommand "cabal sandbox delete" >>= Proc.waitForProcess

         Dir.removeDirectoryRecursive $ buildDir </> "dist"
         Dir.setCurrentDirectory prevDir


tests :: T.TestTree
tests = T.testGroup "Tests" [dropTests, updateTests, dumpTests]


dropTests :: T.TestTree
dropTests = T.testGroup "Drop Tests"
   [ test "DropBothOfAll" defaultDrop
   , test "DropUpperOfAll" $ defaultDrop { upper = True }
   , test "DropBothOfLib" $ defaultDrop { library = True }
   , test "DropUpperOfLib" $ defaultDrop { upper = True, library = True }
   , test "DropBothOfExe" $ defaultDrop { executable = ["cabal-bounds"] }
   , test "DropUpperOfExe" $ defaultDrop { upper = True, executable = ["cabal-bounds"] }
   , test "DropBothOfOtherExe" $ defaultDrop { executable = ["other-exe"] }
   , test "DropUpperOfOtherExe" $ defaultDrop { upper = True, executable = ["other-exe"] }
   , test "DropBothOfAllExes" $ defaultDrop { executable = ["cabal-bounds", "other-exe"] }
   , test "DropUpperOfAllExes" $ defaultDrop { upper = True, executable = ["cabal-bounds", "other-exe"] }
   , test "DropBothOfTest" $ defaultDrop { testSuite = ["some-test"] }
   , test "DropUpperOfTest" $ defaultDrop { upper = True, testSuite = ["some-test"] }
   , test "DropBothOnlyBase" $ defaultDrop { only = ["base"] }
   , test "DropUpperOnlyBase" $ defaultDrop { upper = True, only = ["base"] }
   , test "DropBothIgnoreA" $ defaultDrop { ignore = ["A"] }
   , test "DropUpperIgnoreA" $ defaultDrop { upper = True, ignore = ["A"] }
   ]


updateTests :: T.TestTree
updateTests = T.testGroup "Update Tests"
   [ test "UpdateBothOfAll" defaultUpdate
   , test "UpdateBothOfAll" $ defaultUpdate { lower = True, upper = True }
   , test "UpdateBothOfAllExes" $ defaultUpdate { executable = ["cabal-bounds", "other-exe"] }
   , test "UpdateBothOfExe" $ defaultUpdate { executable = ["cabal-bounds"] }
   , test "UpdateBothOfLibrary" $ defaultUpdate { library = True }
   , test "UpdateBothOfOtherExe" $ defaultUpdate { executable = ["other-exe"] }
   , test "UpdateBothOfTest" $ defaultUpdate { testSuite = ["some-test"] }
   , test "UpdateLowerOfAll" $ defaultUpdate { lower = True }
   , test "UpdateLowerOfAllExes" $ defaultUpdate { lower = True, executable = ["cabal-bounds", "other-exe"] }
   , test "UpdateLowerOfExe" $ defaultUpdate { lower = True, executable = ["cabal-bounds"] }
   , test "UpdateLowerOfLibrary" $ defaultUpdate { lower = True, library = True }
   , test "UpdateLowerOfOtherExe" $ defaultUpdate { lower = True, executable = ["other-exe"] }
   , test "UpdateLowerOfTest" $ defaultUpdate { lower = True, testSuite = ["some-test"] }
   , test "UpdateUpperOfAll" $ defaultUpdate { upper = True }
   , test "UpdateUpperOfAllExes" $ defaultUpdate { upper = True, executable = ["cabal-bounds", "other-exe"] }
   , test "UpdateUpperOfExe" $ defaultUpdate { upper = True, executable = ["cabal-bounds"] }
   , test "UpdateUpperOfLibrary" $ defaultUpdate { upper = True, library = True }
   , test "UpdateUpperOfOtherExe" $ defaultUpdate { upper = True, executable = ["other-exe"] }
   , test "UpdateUpperOfTest" $ defaultUpdate { upper = True, testSuite = ["some-test"] }
   , test "UpdateBothIgnoreA" $ defaultUpdate { ignore = ["A"] }
   , test "UpdateMinorLower" $ defaultUpdate { lowerComp = Just Minor }
   , test "UpdateMajor2Lower" $ defaultUpdate { lowerComp = Just Major2 }
   , test "UpdateMajor1Lower" $ defaultUpdate { lowerComp = Just Major1 }
   , test "UpdateMinorUpper" $ defaultUpdate { upperComp = Just Minor }
   , test "UpdateMajor2Upper" $ defaultUpdate { upperComp = Just Major2 }
   , test "UpdateMajor1Upper" $ defaultUpdate { upperComp = Just Major1 }
   , test "UpdateMinorLowerAndUpper" $ defaultUpdate { lowerComp = Just Minor, upperComp = Just Minor }
   , test "UpdateMajor1LowerAndUpper" $ defaultUpdate { lowerComp = Just Major1, upperComp = Just Major1 }
   , test "UpdateOnlyMissing" $ defaultUpdate { missing = True }
   , test "UpdateByHaskellPlatform" $ defaultUpdate { haskellPlatform = "2013.2.0.0" }
   , test "UpdateUpperFromFile" $ defaultUpdate { upper = True, fromFile = "tests" </> "inputFiles" </> "FromFile.hs" }
   ]


dumpTests :: T.TestTree
dumpTests = T.testGroup "Dump Tests"
   [ test "Dump" $ defaultDump
   ]


test :: String -> Args -> T.TestTree
test testName args =
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
                                }
              Update {} -> args { cabalFile       = Just inputFile
                                , output          = Just outputFile
                                , setupConfigFile = Just setupConfigFile
                                }

              Dump {}   -> args { cabalFiles = [inputFile]
                                , output     = Just outputFile
                                }

      diff ref new    = ["diff", "-u", ref, new]
      goldenFile      = "tests" </> "goldenFiles" </> testName <.> (if isDumpTest then "hs" else "cabal")
      outputFile      = "tests" </> "outputFiles" </> testName <.> (if isDumpTest then "hs" else "cabal")

      inputFile       = "tests" </> "inputFiles" </> (inputFileName testName)
         where
            inputFileName "UpdateByHaskellPlatform" = "hp-original.cabal"
            inputFileName "UpdateOnlyMissing"       = "missing-original.cabal"
            inputFileName _                         = "original.cabal"

      setupConfigFile = "tests" </> "inputFiles"  </> "setup-config"
      isDumpTest      = case args of Dump {} -> True; _ -> False
