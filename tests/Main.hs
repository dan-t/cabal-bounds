
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
updateTests = testGroup "Update Tests" []


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
      inputFile  = "tests" </> "inputFiles"  </> testName <.> "cabal"
