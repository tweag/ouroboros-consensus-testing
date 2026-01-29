{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A 'TestSuite' data structure to arrange 'ConformanceTest' groups
-- designed to interface between property test execution and the
-- conformance testing harness.
-- They are modeled after, and can be compiled to, a tasty 'TestTree'.
module Test.Consensus.Genesis.TestSuite (
    TestSuite
  , get
  , mapKeys
  , mkTestSuite
  , nest
  , toTestTree
  ) where

import           Data.List (partition)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Universe.Class
import           Ouroboros.Consensus.Block (BlockSupportsDiffusionPipelining,
                     ConvertRawHash, Header)
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode)
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Storage.ChainDB (SerialiseDiskConstraints)
import           Ouroboros.Consensus.Storage.LedgerDB.API
                     (CanUpgradeLedgerTables)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Util.ShowProxy
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PeerSimulator.StateView (StateView)
import           Test.Consensus.PointSchedule (HasPointScheduleTestParams)
import           Test.Consensus.PointSchedule.NodeState (NodeState)
import           Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import           Test.Util.TersePrinting (Terse)

data TestSuiteData blk = TestSuiteData
  { -- | The nested groups a test belongs to.
    -- It is 'NonEmpty', as we need a test description/name
    -- in order to produce a 'TestTree'. For this, the
    -- convention is to use the 'NonEmpty.last' and nest
    -- the test by appending.
    prefix :: NonEmpty Prefix

    -- | The test to run.
  , test   :: ConformanceTest blk
  }

type Prefix = String

newtype TestSuite blk key = TestSuite (Map key (TestSuiteData blk))

-- | Smart constructor for a 'TestSuite'.
mkTestSuite :: (Ord key, Finite key)
            => (key -> Prefix)
            -> (key -> ConformanceTest blk)
            -> TestSuite blk key
mkTestSuite toDescription toTest = TestSuite . Map.fromList $
  [ (k, tsData)
  | k <- universeF
  , let tsData = TestSuiteData { prefix = [toDescription k]
                               , test = toTest k
                               }
  ]

mapKeys :: Ord b => (a -> b) -> TestSuite blk a -> TestSuite blk b
mapKeys f (TestSuite m) = TestSuite $ Map.mapKeys f m

get :: Ord key => TestSuite blk key -> key ->  ConformanceTest blk
get (TestSuite m) k = test $ case Map.lookup k m of
  Just t  -> t
  Nothing -> error "TestSuite.get: Impossible! All test classes have a value by contruction."

nest :: [Prefix] -> TestSuite blk key -> TestSuite blk key
nest pfs (TestSuite m) = TestSuite $
  Map.map (\testData ->
             testData {prefix = NonEmpty.prependList pfs $ prefix testData}) m

-- | Produces a single test tasty 'TestTree', along with its containing
-- groups prefixes, out a 'TestSuiteData'.
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
  TestSuiteData blk -> ([Prefix], TestTree)
compileSingleTest (TestSuiteData {prefix, test}) =
  -- The 'last' element of the 'prefix' corresponds to the individual test
  -- description/name. Conversely, the 'init' corresponds to the enclosing
  -- test groups.
  let (prefixes,testName) = (NonEmpty.init prefix, NonEmpty.last prefix)
   in (prefixes, QC.testProperty testName (runConformanceTest test))

-- | Recursively build test groups by following the hierarchy described by
-- the '[Prefix]'. The resulting '[TestTree]' are considered to be part
-- of the same top level group.
buildGroups :: [([Prefix], TestTree)] -> [TestTree]
buildGroups entries =
  let
    -- Distinguish between tests that belong to the top level
    -- from the nested ones.
    (toplevel, nested) = partition (null . fst) entries

    topLevelTests :: [TestTree]
    topLevelTests = fmap snd toplevel

    -- Group the nested tests by their first prefix.
    grouped :: Map Prefix [([Prefix], TestTree)]
    grouped =
      Map.fromListWith (<>)
        [ (p, [(ps, t)])
        | (p:ps, t) <- nested
        ]

    subgroups :: [TestTree]
    subgroups =
      [ testGroup p (buildGroups xs)
      | (p, xs) <- Map.toList grouped
      ]
  in
    topLevelTests <> subgroups

-- | Compile a 'TestSuite' into a tasty 'TestTree', with the given 'Prefix'
-- as the top level description/name.
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
  Prefix -> TestSuite blk key -> TestTree
toTestTree p (TestSuite m) =
  let leafs = fmap snd $ Map.toAscList m
      entries = fmap compileSingleTest leafs
   in testGroup p $ buildGroups entries
