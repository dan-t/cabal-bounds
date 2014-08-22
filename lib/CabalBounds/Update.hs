module CabalBounds.Update
   ( update
   , Libraries
   ) where

import qualified Distribution.PackageDescription as D
import qualified Distribution.Package as P
import qualified Distribution.Version as V
import Control.Lens
import CabalBounds.Bound (UpdateBound(..))
import CabalBounds.Dependencies (Dependencies(..), filterDependency)
import CabalBounds.VersionComp (VersionComp(..))
import qualified CabalLenses as CL
import Data.List (foldl')
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

type PkgName    = String
type LibName    = String
type LibVersion = V.Version
type Libraries  = HM.HashMap LibName LibVersion


update :: UpdateBound -> [CL.Section] -> Dependencies -> Libraries -> D.GenericPackageDescription -> D.GenericPackageDescription
update bound sections deps libs pkgDescrp =
   foldl' updateSection pkgDescrp sections
   where
      updateSection pkgDescrp section =
         pkgDescrp & CL.dependencyIf condVars section . filterDep %~ updateDep

      filterDep = filterDependency deps
      updateDep = updateDependency bound libs
      condVars  = CL.fromDefaults pkgDescrp


updateDependency :: UpdateBound -> Libraries -> P.Dependency -> P.Dependency
updateDependency (UpdateLower comp ifMissing) libs dep =
   fromMaybe dep $
      if ifMissing && lowerBound_ /= CL.noLowerBound
         then return dep
         else do
            version <- HM.lookup pkgName_ libs
            let newLowerVersion = comp `compOf` version
                newLowerBound   = V.LowerBound newLowerVersion V.InclusiveBound
                vrange          = fromMaybe (V.orLaterVersion newLowerVersion)
                                            (modifyVersionIntervals (updateLower newLowerBound) versionRange_)
            return $ mkDependency pkgName_ vrange
   where
      updateLower newLowerBound []        = [(newLowerBound, V.NoUpperBound)]
      updateLower newLowerBound intervals = intervals & _head . CL.lowerBound .~ newLowerBound

      pkgName_      = pkgName dep
      versionRange_ = versionRange dep
      lowerBound_   = fromMaybe CL.noLowerBound $ V.asVersionIntervals versionRange_ ^? _head . CL.lowerBound

updateDependency (UpdateUpper comp ifMissing) libs dep =
   fromMaybe dep $
      if ifMissing && upperBound_ /= V.NoUpperBound
         then return dep
         else do
            upperVersion <- HM.lookup pkgName_ libs
            let newUpperVersion = comp `compOf` upperVersion
                newUpperBound   = V.UpperBound (nextVersion newUpperVersion) V.ExclusiveBound
            vrange <- modifyVersionIntervals (updateUpper newUpperBound) versionRange_
            return $ mkDependency pkgName_ vrange
   where
      versionRange_ = versionRange dep
      pkgName_      = pkgName dep
      upperBound_   = fromMaybe V.NoUpperBound $ V.asVersionIntervals versionRange_ ^? _head . CL.upperBound

      updateUpper newUpperBound []        = [(CL.noLowerBound, newUpperBound)]
      updateUpper newUpperBound intervals = intervals & _last . CL.upperBound .~ newUpperBound

updateDependency (UpdateBoth lowerComp upperComp ifMissing) libs dep =
    updateDependency (UpdateLower lowerComp ifMissing) libs $
    updateDependency (UpdateUpper upperComp ifMissing) libs dep


modifyVersionIntervals :: ([V.VersionInterval] -> [V.VersionInterval]) -> V.VersionRange -> Maybe V.VersionRange
modifyVersionIntervals f = fmap V.fromVersionIntervals . V.mkVersionIntervals . f . V.asVersionIntervals


compOf :: VersionComp -> V.Version -> V.Version
Major1 `compOf` version =
   version & CL.versionBranchL %~ (take 1 . ensureMinimalVersionBranch Major1)
           & CL.versionTagsL   .~ []

Major2 `compOf` version =
   version & CL.versionBranchL %~ (take 2 . ensureMinimalVersionBranch Major2)
           & CL.versionTagsL   .~ []

Minor `compOf` version =
   version & CL.versionBranchL %~ ensureMinimalVersionBranch Minor
           & CL.versionTagsL   .~ []


ensureMinimalVersionBranch :: VersionComp -> [Int] -> [Int]
ensureMinimalVersionBranch comp branch =
   let numDigits  = numNeededVersionDigits comp
       numMissing = numDigits - length branch
       branch' | numMissing > 0 = branch ++ replicate numMissing 0
               | otherwise      = branch
       in branch'
   where
      numNeededVersionDigits Major1 = 1
      numNeededVersionDigits Major2 = 2
      numNeededVersionDigits Minor  = 3


nextVersion :: V.Version -> V.Version
nextVersion version = version & CL.versionBranchL . _last %~ (+ 1)


pkgName :: P.Dependency -> PkgName
pkgName (P.Dependency (P.PackageName name) _) = name


versionRange :: P.Dependency -> V.VersionRange
versionRange (P.Dependency _ vrange) = vrange


mkDependency :: PkgName -> V.VersionRange -> P.Dependency
mkDependency name = P.Dependency (P.PackageName name)
