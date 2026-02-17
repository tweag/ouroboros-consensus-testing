{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A 'TestSuite' data structure for quick access to 'ConformanceTest' values.
-- It encodes a hierarchical nested structure allowing it to compile into a
-- tasty 'TestTree'.
-- It's purpose is interfacing between property test execution and the
-- conformance testing harness.
module Test.Consensus.Genesis.TestSuite (
    Finite
  , Generic
  , GenericUniverse (..)
  , TestSuite
  , Universe
  , get
  , group
  , mkTestSuite
  , toTestTree
  ) where

import           Data.Coerce (coerce)
import           Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Endo (..))
import           Data.Universe.Class (Finite (..), Universe (..))
import           Data.Universe.Generic (GUniverse, universeGeneric)
import           GHC.Generics (Generic (Rep), Generically (..))
import           Ouroboros.Consensus.Block (BlockSupportsDiffusionPipelining,
                     ConvertRawHash, Header)
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode)
import           Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory)
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import           Ouroboros.Consensus.Storage.LedgerDB.API
                     (CanUpgradeLedgerTables)
import           Ouroboros.Consensus.Util.Condense (Condense, CondenseList)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)
import           Test.Consensus.Genesis.Setup (ConformanceTest (..),
                     runConformanceTest)
import           Test.Consensus.PeerSimulator.StateView (StateView)
import           Test.Consensus.PointSchedule (HasPointScheduleTestParams)
import           Test.Consensus.PointSchedule.NodeState (NodeState)
import           Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import           Test.Util.TersePrinting (Terse)

-- | A data type with generically defined  'Universe' and 'Finite' instances.
-- Intended to derive said instances (via @DerivingVia@ extension) for 'TestSuite' keys.
newtype GenericUniverse a = GenericUniverse a

instance (Generic a, GUniverse (Rep a)) => Universe (GenericUniverse a) where
  universe = coerce $ universeGeneric @a

instance (Generic a, GUniverse (Rep a)) => Finite (GenericUniverse a)

data TestSuiteData blk = TestSuiteData
  { -- | A prefix representing a path through the test group tree.
    -- The convention is to nest by appending, i.e. the head of the prefix
    -- corresponds to the top-level test group.
    tsPrefix :: [String]

    -- | The test itself.
  , tsTest   :: ConformanceTest blk
  }

-- | A @TestSuite blk key@ contains one 'ConformanceTest'@blk@ for each @key@.
newtype TestSuite blk key = TestSuite (Map key (TestSuiteData blk))

-- | Build a 'TestSuite' from a function mapping a @key@ type to 'ConformanceTest'.
-- 'Universe' and 'Finite' constraints on @key@ are meant to be derived
-- generically by means of the 'GenericUniverse' wrapper.
-- NOTE: Using a @key@ having a constructor with a big finite type parameter
-- (such as 'Int') should be avoided as this is likely to flood the memory.
-- TODO: Reimplement 'Finite' to prevent instances of big finite types.
mkTestSuite :: (Ord key, Finite key)
            => (key -> ConformanceTest blk)
            -> TestSuite blk key
mkTestSuite toConformanceTest = TestSuite . Map.fromList $ do
  k <- universeF
  let test = toConformanceTest k
      tsData = TestSuiteData { tsPrefix = []
                             , tsTest = test
                             }
  pure (k, tsData)

get :: Ord key => TestSuite blk key -> key ->  ConformanceTest blk
get (TestSuite m) k = tsTest $ case Map.lookup k m of
  Just t  -> t
  Nothing -> error "TestSuite.get: Impossible! A TestSuite is a total map."

-- | Appends the given string to the prefix of all tests.
group :: String -> TestSuite blk key -> TestSuite blk key
group pfs (TestSuite m) = TestSuite $
  Map.map (\testData ->
             testData {tsPrefix = pfs : tsPrefix testData}) m

-- * Compile 'TestSuite' into a 'TestTree'

-- | Intermediary representation for a 'TestSuite' to be compiled into a 'TestTree'.
data TestTrie = TestTrie
  { _here     :: ![TestTree] -- ^ Top level tests (whose prefix ends here).
  , _children :: !(MonoidalMap String TestTrie) -- ^ Grouped tests correspond to prefix maps.
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically TestTrie)

-- | Create a 'TestTrie' with a single value.
mkTestTrie :: [String] -> TestTree -> TestTrie
mkTestTrie pfs t =
  let nest :: String -> Endo TestTrie
      nest pf = Endo $ \tt -> TestTrie [] (MMap.singleton pf tt)
      leaf = TestTrie [t] mempty
  in appEndo (foldMap nest pfs) leaf

-- | Fold a list of prefixed tests into a 'TestTrie'.
--
-- Each input pair @([Prefix], TestTree)@ is interpreted as a path
-- in the trie, and the resulting trie merges common prefixes into
-- shared nodes.
buildTrie :: [([String], TestTree)] -> TestTrie
buildTrie = foldMap (uncurry mkTestTrie)

-- | Fold a 'TestTrie' into a list of 'TestTree's by recursively
-- rendering each trie node as a 'testGroup'.
render :: TestTrie -> [TestTree]
render (TestTrie here children) =
  here <>
    fmap
      (\(p,tt) -> testGroup p (render tt))
      (MMap.toList children)

-- | Produces a single-test 'TestTree', along with its containing
-- 'group' prefixes, out a 'TestSuiteData'.
compileSingleTest ::
  ( Condense (StateView blk)
  , CondenseList (NodeState blk)
  , ShowProxy blk
  , ShowProxy (Header blk)
  , ConfigSupportsNode blk
  , LedgerSupportsProtocol blk
  , SerialiseDiskConstraints blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  , CanUpgradeLedgerTables (LedgerState blk)
  , HasPointScheduleTestParams blk
  , Eq (Header blk)
  , Eq blk
  , Terse blk
  , Condense (NodeState blk)
  ) =>
  TestSuiteData blk -> ([String], TestTree)
compileSingleTest (TestSuiteData {tsPrefix, tsTest}) =
  let testName = ctDescription tsTest
   in (tsPrefix, QC.testProperty testName (runConformanceTest tsTest))

-- | Compile a 'TestSuite' into a tasty 'TestTree', with the given 'String'
-- as the top level description/name of the resulting group.
toTestTree ::
  ( Condense (StateView blk)
  , CondenseList (NodeState blk)
  , ShowProxy blk
  , ShowProxy (Header blk)
  , ConfigSupportsNode blk
  , LedgerSupportsProtocol blk
  , SerialiseDiskConstraints blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  , CanUpgradeLedgerTables (LedgerState blk)
  , HasPointScheduleTestParams blk
  , Eq (Header blk)
  , Eq blk
  , Terse blk
  , Condense (NodeState blk)
  ) =>
  String -> TestSuite blk key -> TestTree
toTestTree p (TestSuite m) =
  let tests = fmap snd $ Map.toList m
      prefixedTests = fmap compileSingleTest tests
   in testGroup p . render . buildTrie $ prefixedTests

-- | Applies a function to all conformance tests in a @TestSuite@.
adjustTestSuite
  :: (ConformanceTest blk -> ConformanceTest blk)
  -> TestSuite blk key -> TestSuite blk key
adjustTestSuite = adjustTestSuiteWithKey (const True)

-- | Similar to @adjustTestSuite@, but the adjustment has access to the @key@.
adjustTestSuiteWithKey
  :: (key -> ConformanceTest blk -> ConformanceTest blk)
  -> TestSuite blk key -> TestSuite blk key
adjustTestSuiteWithKey adjust (TestSuite m) = TestSuite $
  Map.mapWithKey (\k testData -> testData {tsTest = adjust k (tsTest testData)}) m

-- | Apply an adjustment to the maximum test case size of a conformance test.
-- Analogous to @Test.QuickCheck.withMaxSize@. Unless the previous size is less
-- than or equal to zero, the new maximum size will be at least 1.
adjustMaxSize :: (Int -> Int) -> TestSuite blk key -> TestSuite blk key
adjustMaxSize = adjustMaxSizeWithKey (const True)

-- | Similar to @adjustMaxSize@, but the adjustment has access
-- to the @key@.
adjustMaxSizeWithKey
  :: (key -> Int -> Int) -> TestSuite blk key -> TestSuite blk key
adjustMaxSizeWithKey f = adjustTestSuiteWithKey
  (\k ct -> ct {ctMaxSize = atLeastOneIfNotZero (f k (ctMaxSize ct))})
  where
    atLeastOneIfNotZero :: Int -> Int
    atLeastOneIfNotZero n
      | n <= 0    = 0
      | otherwise = max 1 n

-- | Apply an adjustment to the desired number of passing test cases for
-- a conformance test.
adjustDesiredPasses :: (Int -> Int) -> TestSuite blk key -> TestSuite blk key
adjustDesiredPasses = adjustDesiredPassesWithKey (const True)

-- | Similar to @adjustDesiredPasses@, but the adjustment has access
-- to the @key@.
adjustDesiredPassesWithKey
  :: (key -> Int -> Int) -> TestSuite blk key -> TestSuite blk key
adjustDesiredPassesWithKey keyPred f = adjustTestSuiteWithKey
  (\k ct -> ct {ctDesiredPasses = atLeastOneIfNotZero (keyPred k (ctDesiredPasses ct))})
  where
    atLeastOneIfNotZero :: Int -> Int
    atLeastOneIfNotZero n
      | n <= 0    = 0
      | otherwise = max 1 n
