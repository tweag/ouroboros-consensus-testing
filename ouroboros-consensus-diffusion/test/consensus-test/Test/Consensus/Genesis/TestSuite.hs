{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A 'TestSuite' data structure for quick access to 'ConformanceTest' valuesa
-- providing facilities to arrange them in nested hierarchical groups.
-- It's purpose is interfacing between property test execution and the
-- conformance testing harness.
module Test.Consensus.Genesis.TestSuite (
    Generic
  , GenericUniverse (..)
  , TestSuite
  , get
  , group
  , mkTestSuite
  , toTestTree
  ) where

import           Data.Coerce (coerce)
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Universe.Class (Finite (..), Universe (..))
import           Data.Universe.Generic (GUniverse, universeGeneric)
import           GHC.Generics (Generic (Rep))
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

-- | Build a 'TestSuite' from a mapping function.
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
  { _here     :: [TestTree] -- ^ Top level tests (whose prefix ends here).
  , _children :: Map String TestTrie -- ^ Grouped tests correspond to prefix maps.
  }

insert :: [String] -> TestTree -> TestTrie -> TestTrie
insert [] t (TestTrie ts ch) = TestTrie (t:ts) ch
insert (p:ps) t (TestTrie ts ch) =
  let go :: Maybe TestTrie -> Maybe TestTrie
      go Nothing  = Just (insert ps t (TestTrie [] Map.empty))
      go (Just c) = Just (insert ps t c)
   in TestTrie ts (Map.alter go p ch)

buildTrie :: [([String], TestTree)] -> TestTrie
buildTrie = foldl' (\tt (ps, t) -> insert ps t tt) (TestTrie [] Map.empty)

render :: TestTrie -> [TestTree]
render (TestTrie here children) =
  here <> fmap (\(p,tt) -> testGroup p (render tt)) (Map.toList children)

-- | Produces a single-test tasty 'TestTree', along with its containing
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
  -- The 'last' element of the 'tcPrefix' corresponds to the individual test
  -- description/name. Conversely, the 'init' corresponds to the enclosing
  -- test groups.
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
