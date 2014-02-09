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
                           (targets (A.executable a) (A.library a) (A.testSuite a) (A.benchmark a))

command a@A.Update {} = Update (bound (A.lower a) (A.upper a))
                               (targets (A.executable a) (A.library a) (A.testSuite a) (A.benchmark a))


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

data Target = Executable String
            | Library String
            | TestSuite String
            | Benchmark String
            deriving (Show, Eq)

targets :: [String] -> [String] -> [String] -> [String] -> Targets
targets exes libs tests benchs
   | ts@(_:_) <- concat [ map Executable exes
                        , map Library libs
                        , map TestSuite tests
                        , map Benchmark benchs
                        ]
   = Targets ts

   | otherwise
   = AllTargets
