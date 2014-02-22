
module Main where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath
import System.IO (hPutStrLn, stderr)
import qualified CabalBounds.Args as Args
import CabalBounds.Main (cabalBounds)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dropTests, updateTests]

dropTests :: TestTree
dropTests = testGroup "Drop Tests"
   [ dropTest "DropBothOfAll" False False [] [] []
   , dropTest "DropUpperOfAll" True False [] [] []
   , dropTest "DropBothOfLib" False True [] [] []
   , dropTest "DropUpperOfLib" True True [] [] []
   , dropTest "DropBothOfExe" False False ["cabal-bounds"] [] []
   , dropTest "DropUpperOfExe" True False ["cabal-bounds"] [] []
   , dropTest "DropBothOfOtherExe" False False ["other-exe"] [] []
   , dropTest "DropUpperOfOtherExe" True False ["other-exe"] [] []
   , dropTest "DropBothOfAllExes" False False ["cabal-bounds", "other-exe"] [] []
   , dropTest "DropUpperOfAllExes" True False ["cabal-bounds", "other-exe"] [] []
   , dropTest "DropBothOfTest" False False [] ["some-test"] []
   , dropTest "DropUpperOfTest" True False [] ["some-test"] []
   ]


updateTests :: TestTree
updateTests = testGroup "Update Tests"
   [ updateTest "UpdateBothOfAll" False False False [] [] []
   , updateTest "UpdateBothOfAll" True True False [] [] []
   , updateTest "UpdateBothOfAllExes" True True False ["cabal-bounds", "other-exe"] [] []
   , updateTest "UpdateBothOfExe" True True False ["cabal-bounds"] [] []
   , updateTest "UpdateBothOfLibrary" True True True [] [] []
   , updateTest "UpdateBothOfOtherExe" True True False ["other-exe"] [] []
   , updateTest "UpdateBothOfTest" True True False [] ["some-test"] []
   , updateTest "UpdateLowerOfAll" True False False [] [] []
   , updateTest "UpdateLowerOfAllExes" True False False ["cabal-bounds", "other-exe"] [] []
   , updateTest "UpdateLowerOfExe" True False False ["cabal-bounds"] [] []
   , updateTest "UpdateLowerOfLibrary" True False True [] [] []
   , updateTest "UpdateLowerOfOtherExe" True False False ["other-exe"] [] []
   , updateTest "UpdateLowerOfTest" True False False [] ["some-test"] []
   , updateTest "UpdateUpperOfAll" False True False [] [] []
   , updateTest "UpdateUpperOfAllExes" False True False ["cabal-bounds", "other-exe"] [] []
   , updateTest "UpdateUpperOfExe" False True False ["cabal-bounds"] [] []
   , updateTest "UpdateUpperOfLibrary" False True True [] [] []
   , updateTest "UpdateUpperOfOtherExe" False True False ["other-exe"] [] []
   , updateTest "UpdateUpperOfTest" False True False [] ["some-test"] []
   ] 


updateTest :: String -> Bool -> Bool -> Bool -> [String] -> [String] -> [String] -> TestTree 
updateTest testName lower upper lib exes tests benchms =
   goldenVsFileDiff testName diff goldenFile outputFile command
   where
      command = do
         let args = Args.Update { Args.lower           = lower
                                , Args.upper           = upper
                                , Args.library         = lib
                                , Args.executable      = exes
                                , Args.testSuite       = tests
                                , Args.benchmark       = benchms
                                , Args.cabalFile       = inputFile
                                , Args.outputCabalFile = outputFile
                                , Args.setupConfigFile = setupConfigFile
                                }

         error <- cabalBounds args
         case error of
              Just err -> hPutStrLn stderr ("cabal-bounds: " ++ err)
              _        -> return ()


      diff ref new = ["diff", "-u", ref, new]

      goldenFile      = "tests" </> "goldenFiles" </> testName <.> "cabal"
      outputFile      = "tests" </> "outputFiles" </> testName <.> "cabal"
      inputFile       = "tests" </> "inputFiles"  </> "original.cabal"
      setupConfigFile = "tests" </> "inputFiles"  </> "setup-config"


dropTest :: String -> Bool -> Bool -> [String] -> [String] -> [String] -> TestTree
dropTest testName upper lib exes tests benchms =
   goldenVsFileDiff testName diff goldenFile outputFile command
   where
      command = do
         let args = Args.Drop { Args.upper           = upper
                              , Args.library         = lib
                              , Args.executable      = exes
                              , Args.testSuite       = tests
                              , Args.benchmark       = benchms
                              , Args.cabalFile       = inputFile
                              , Args.outputCabalFile = outputFile
                              }

         error <- cabalBounds args
         case error of
              Just err -> hPutStrLn stderr ("cabal-bounds: " ++ err)
              _        -> return ()


      diff ref new = ["diff", "-u", ref, new]

      goldenFile = "tests" </> "goldenFiles" </> testName <.> "cabal"
      outputFile = "tests" </> "outputFiles" </> testName <.> "cabal"
      inputFile  = "tests" </> "inputFiles"  </> "original.cabal"
