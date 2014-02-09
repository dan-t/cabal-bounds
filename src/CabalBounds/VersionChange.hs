
module CabalVersions.VersionChange where

type Name = String
-- | the target for the version change
data Target = AllTargets
            | Executable Name
            | Library Name
            | TestSuite Name
            | Benchmark Name
            deriving (Show, Eq)


-- | the version bound
data Bound = UpperBound
           | LowerBound
           | AnyBound
           deriving (Show, Eq)


data VersionChange = Clear Bound Target
                   | 
