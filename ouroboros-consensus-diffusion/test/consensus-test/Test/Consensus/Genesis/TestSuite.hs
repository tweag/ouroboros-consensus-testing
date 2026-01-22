{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
-- | A 'TestSuite' data structure to arrange 'ConformanceTest' groups
-- designed to interface between property test execution and the
-- conformance testing harness.
-- They are modeled after, and can be compiled to, a tasty 'TestTree'.
module Test.Consensus.Genesis.TestSuite (
    TestSuite
  , allTheTests
  , lookup
  , mkTestTree
  , singleton
  , toList
  ) where

import           Data.Bifunctor (first)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Prelude hiding (lookup)
import           Test.Consensus.Genesis.Setup (ConformanceTest)
import           Test.Tasty
import           Test.Tasty.QuickCheck

-- | Sum type of all the existing (genesis) test properties
-- TODO: Populate this type with the ported tests.
data TestProperty = TestProperty deriving (Eq, Ord, Show)

-- | This type is analogous to a 'Tasty.Test.TestName'.
type Description = String

-- | A /path/ through the nested hierarchy of a 'TestSuite';
-- The notion of /test group/ consists of all 'TestSuite' values sharing a 'TestHierarchy'
type TestHierarchy = NonEmpty Description

-- | A 'TestSuite' is a collection of test arranged in (nested) groups.
--
-- INVARIANT: A 'TestProperty' uniquely identifies a value in a 'TestSuite'.
newtype TestSuite a = TestSuite (Map (TestHierarchy, TestProperty) a)
  deriving (Eq, Semigroup, Monoid)

lookup :: TestProperty -> TestSuite a -> Maybe a
lookup prop (TestSuite testMap) = Map.lookup prop $ Map.mapKeys snd testMap

-- | A singleton 'TestSuite' corresponds to a leaf in the test tree.
singleton :: Description -> TestProperty -> a -> TestSuite a
singleton pf prop test = TestSuite $ Map.singleton ([pf],prop) test

nest :: TestHierarchy -> TestSuite a -> TestSuite a
nest h (TestSuite testMap) = TestSuite $ Map.mapKeys (first (h <>)) testMap

addTestSuiteToGroup :: TestHierarchy -> TestSuite a -> TestSuite a -> TestSuite a
addTestSuiteToGroup h suite = (nest h suite <>)

pickGroup :: TestHierarchy -> TestSuite a -> TestSuite a
pickGroup h0 (TestSuite testMap) =
  TestSuite $ Map.filterWithKey (\(h, _) _ ->
                                   NonEmpty.isPrefixOf (NonEmpty.toList h0) h
                                )
                                testMap


toList :: TestSuite a -> [((TestHierarchy, TestProperty), a)]
toList (TestSuite testMap)= Map.toList $ testMap

mkSingleTest :: Testable a => TestHierarchy -> a -> TestTree
mkSingleTest hierarchy t = testProperty (NonEmpty.head hierarchy) t

allSingleTests :: Testable a => TestSuite a -> [TestTree]
allSingleTests (TestSuite testMap) =
  Map.elems $ Map.mapWithKey (\(h,_) -> mkSingleTest h) testMap

-- | Compile a 'TestSuite' to a tasty 'TestTree'.
mkTestTree :: TestSuite a -> TestTree
mkTestTree (TestSuite testMap)= undefined

allTheTests :: TestSuite (ConformanceTest blk)
allTheTests = undefined
