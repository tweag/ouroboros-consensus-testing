{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A 'TestSuite' data structure to arrange 'ConformanceTest' groups
-- designed to interface between property test execution and the
-- conformance testing harness.
-- They are modeled after, and can be compiled to, a tasty 'TestTree'.
module Test.Consensus.Genesis.TestSuite where

import           Data.List.Extra (groupSortOn)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.Consensus.Genesis.Setup (ConformanceTest)
import           Test.Tasty
import           Test.Tasty.QuickCheck

data TestSuiteData blk = TestSuiteData
  { prefix :: NonEmpty Prefix
  , test   :: ConformanceTest blk
  }

type Prefix = String

newtype TestSuite blk key = TestSuite (Map key (TestSuiteData blk))
  deriving newtype (Semigroup, Monoid)

mkTestSuite :: Prefix -> key -> ConformanceTest blk -> TestSuite blk key
mkTestSuite p k t = TestSuite . Map.singleton k $
  TestSuiteData { prefix = [p]
                , test = t
                }

map :: Ord b => (a -> b) -> TestSuite blk a -> TestSuite blk b
map f (TestSuite m) = TestSuite $ Map.mapKeys f m

get :: Ord key => TestSuite blk key -> key ->  ConformanceTest blk
get (TestSuite m) k = test $ case Map.lookup k m of
  Just t  -> t
  Nothing -> error "TestSuite.get: Impossible! All test classes have a value."

nest :: [Prefix] -> TestSuite blk key -> TestSuite blk key
nest pfs (TestSuite m) = TestSuite $
  Map.map (\testData ->
             testData {prefix = NonEmpty.prependList pfs $ prefix testData}) m

toTestTree :: TestSuite blk key -> TestTree
toTestTree (TestSuite m) = undefined
