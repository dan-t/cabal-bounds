
module CabalBounds.Bound
   ( Bound(..)
   , bound
   ) where   

import CabalBounds.Args

data Bound = LowerBound
           | UpperBound
           | BothBounds
           deriving (Show, Eq)

bound :: Args -> Bound
bound Drop {upper = upper} = if upper then UpperBound else BothBounds

bound Update {lower = lower, upper = upper}
   | lower && upper = BothBounds
   | lower          = LowerBound
   | upper          = UpperBound
   | otherwise      = BothBounds
