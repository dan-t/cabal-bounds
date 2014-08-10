
module Main where

import qualified Test.Tasty as T
import qualified Test.Tasty.Golden as G
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>), (<.>))
import CabalBounds.Args
import CabalBounds.Main (cabalBounds)
import CabalBounds.VersionComp (VersionComp(..))

main = T.defaultMain tests

tests :: T.TestTree
tests = T.testGroup "Tests" [dropTests, updateTests]

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
   , test "DropBothIgnoreBase" $ defaultDrop { ignore = ["base"] }
   , test "DropUpperIgnoreBase" $ defaultDrop { upper = True, ignore = ["base"] }
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
   , test "UpdateBothOnlyBase" $ defaultUpdate { only = ["base"] }
   , test "UpdateUpperOnlyBase" $ defaultUpdate { upper = True, only = ["base"] }
   , test "UpdateBothIgnoreBase" $ defaultUpdate { ignore = ["base"] }
   , test "UpdateLowerIgnoreBase" $ defaultUpdate { lower = True, ignore = ["base"] }
   , test "UpdateMinorLower" $ defaultUpdate { lowerComp = Just Minor }
   , test "UpdateMajor2Lower" $ defaultUpdate { lowerComp = Just Major2 }
   , test "UpdateMajor1Lower" $ defaultUpdate { lowerComp = Just Major1 }
   , test "UpdateMinorUpper" $ defaultUpdate { upperComp = Just Minor }
   , test "UpdateMajor2Upper" $ defaultUpdate { upperComp = Just Major2 }
   , test "UpdateMajor1Upper" $ defaultUpdate { upperComp = Just Major1 }
   , test "UpdateMinorLowerAndUpper" $ defaultUpdate { lowerComp = Just Minor, upperComp = Just Minor }
   , test "UpdateMajor1LowerAndUpper" $ defaultUpdate { lowerComp = Just Major1, upperComp = Just Major1 }
   , test "UpdateOnlyMissing" $ defaultUpdate { missing = True }
   , testWithoutSetupConfig "UpdateByHaskellPlatform" $ defaultUpdate { haskellPlatform = "2013.2.0.0" }
   , test "UpdateByHaskellPlatformAndSetupConfig" $ defaultUpdate { haskellPlatform = "2013.2.0.0" }
   , testWithoutSetupConfig "FromFile" $ defaultUpdate { upper = True, fromFile = "tests" </> "inputFiles" </> "FromFile.hs" }
   , testWithoutSetupConfig "Dump" $ defaultDump
   ]


test :: String -> Args -> T.TestTree
test testName args = test_ testName args True


testWithoutSetupConfig :: String -> Args -> T.TestTree
testWithoutSetupConfig testName args = test_ testName args False


test_ :: String -> Args -> Bool -> T.TestTree
test_ testName args withSetupConfig =
   G.goldenVsFileDiff testName diff goldenFile outputFile command
   where
      command = do
         error <- cabalBounds argsWithFiles
         case error of
              Just err -> hPutStrLn stderr ("cabal-bounds: " ++ err)
              _        -> return ()

      argsWithFiles =
         case args of
              Drop {}   -> args { cabalFile = inputFile
                                , output    = outputFile
                                }
              Update {} -> args { cabalFile       = inputFile
                                , output          = outputFile
                                , setupConfigFile = [setupConfigFile | withSetupConfig]
                                }

              Dump {}   -> args { cabalFiles = [inputFile]
                                , output     = outputFile
                                }

      diff ref new    = ["diff", "-u", ref, new]
      goldenFile      = "tests" </> "goldenFiles" </> testName <.> (if isDumpTest then "hs" else "cabal")
      outputFile      = "tests" </> "outputFiles" </> testName <.> (if isDumpTest then "hs" else "cabal")
      inputFile       = "tests" </> "inputFiles"  </> "original.cabal"
      setupConfigFile = "tests" </> "inputFiles"  </> "setup-config"
      isDumpTest      = case args of Dump {} -> True; _ -> False
