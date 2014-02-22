
module Main where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath
import System.IO (hPutStrLn, stderr)
import CabalBounds.Args
import CabalBounds.Main (cabalBounds)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dropTests, updateTests]

dropTests :: TestTree
dropTests = testGroup "Drop Tests"
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
   ]


updateTests :: TestTree
updateTests = testGroup "Update Tests"
   [ test "UpdateBothOfAll" $ defaultUpdate
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
   ] 


test :: String -> Args -> TestTree
test testName args =
   goldenVsFileDiff testName diff goldenFile outputFile command
   where
      command = do
         error <- cabalBounds argsWithFiles
         case error of
              Just err -> hPutStrLn stderr ("cabal-bounds: " ++ err)
              _        -> return ()

      argsWithFiles = 
         case args of
              Drop {}   -> args { cabalFile       = inputFile
                                , outputCabalFile = outputFile
                                }
              Update {} -> args { cabalFile       = inputFile
                                , outputCabalFile = outputFile
                                , setupConfigFile = setupConfigFile
                                }

      diff ref new    = ["diff", "-u", ref, new]
      goldenFile      = "tests" </> "goldenFiles" </> testName <.> "cabal"
      outputFile      = "tests" </> "outputFiles" </> testName <.> "cabal"
      inputFile       = "tests" </> "inputFiles"  </> "original.cabal"
      setupConfigFile = "tests" </> "inputFiles"  </> "setup-config"
