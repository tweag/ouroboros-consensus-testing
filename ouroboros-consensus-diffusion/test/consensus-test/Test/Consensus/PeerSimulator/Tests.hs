{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.PeerSimulator.Tests (
    testSuite
  , tests
  ) where

import           Ouroboros.Consensus.Block.Abstract (HasHeader, Header,
                     HeaderHash)
import           Ouroboros.Consensus.Util.Condense (Condense)
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.TestSuite
import qualified Test.Consensus.PeerSimulator.Tests.LinkedThreads as LinkedThreads
import qualified Test.Consensus.PeerSimulator.Tests.Rollback as Rollback
import qualified Test.Consensus.PeerSimulator.Tests.Timeouts as Timeouts
import           Test.Tasty
import           Test.Util.TestBlock (TestBlock)

tests :: TestTree
tests = testGroup "PeerSimulator" $ toTestTree @TestBlock testSuite

data Test = LinkedThreads LinkedThreads.Test
          | Rollback Rollback.Test
          | Timeouts Timeouts.Test
  deriving stock (Eq, Ord, Generic)
  deriving (Universe, Finite) via GenericUniverse Test

testSuite ::
  ( IssueTestBlock blk
  , HasHeader blk
  , HasHeader (Header blk)
  , Condense (HeaderHash blk)
  , Condense (Header blk)
  , Eq blk
  ) => TestSuite blk Test
testSuite = mkTestSuite $ \case
  LinkedThreads t -> at LinkedThreads.testSuite t
  Rollback t -> at Rollback.testSuite t
  Timeouts t -> at Timeouts.testSuite t
