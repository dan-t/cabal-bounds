{-# LANGUAGE PatternGuards #-}

module CabalBounds.Command
   ( Bound(..)
   , Targets(..)
   , Target(..)
   , Command(..)
   , command
   ) where

import qualified CabalBounds.Args as A

data Command = Drop Bound Targets
             | Update Bound Targets 
             deriving (Show, Eq)

command :: A.Args -> Command
command a@A.Drop {} = Drop (if A.upper a then UpperBound else BothBounds)
                           (targets (A.library a) (A.executable a) (A.testSuite a) (A.benchmark a))

command a@A.Update {} = Update (bound (A.lower a) (A.upper a))
                               (targets (A.library a) (A.executable a) (A.testSuite a) (A.benchmark a))


data Bound = LowerBound
           | UpperBound
           | BothBounds
           deriving (Show, Eq)

bound :: Bool -> Bool -> Bound
bound lower upper
   | lower && upper = BothBounds
   | lower          = LowerBound
   | upper          = UpperBound
   | otherwise      = BothBounds


data Targets = Targets [Target]
             | AllTargets
             deriving (Show, Eq)

data Target = Library
            | Executable String
            | TestSuite String
            | Benchmark String
            deriving (Show, Eq)

targets :: Bool -> [String] -> [String] -> [String] -> Targets
targets lib exes tests benchs
   | ts@(_:_) <- concat [ if lib then [Library] else []
                        , map Executable exes
                        , map TestSuite tests
                        , map Benchmark benchs
                        ]
   = Targets ts

   | otherwise
   = AllTargets
