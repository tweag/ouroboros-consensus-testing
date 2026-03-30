{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Test.Consensus.Genesis.TestSuite.All (
    TestKey
  , testSuite
  ) where

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Ouroboros.Consensus.Block (GetHeader, HasHeader, Header,
                     HeaderHash)
import           Ouroboros.Consensus.Util.Condense (Condense)
import           Test.Consensus.Genesis.Setup.GenChains (IssueTestBlock)
import qualified Test.Consensus.Genesis.Tests as Genesis
import           Test.Consensus.Genesis.TestSuite
import qualified Test.Consensus.PeerSimulator.Tests as PeerSimulator

-- | The type containing all the test keys.
data TestKey = Genesis !Genesis.TestKey
             | PeerSimulator !PeerSimulator.TestKey
  deriving stock (Show, Eq, Ord, Generic)
  deriving SmallKey via Generically TestKey

instance KeyType TestKey where
  toKey = \case
    Genesis k -> superKey "Genesis" k
    PeerSimulator k -> superKey "PeerSimulator" k

instance ToJSON TestKey where
  toJSON = toJSONKeyType

instance FromJSON TestKey where
  parseJSON = parseJSONKeyType

-- | The test suite containing all conformance tests.
testSuite ::
  ( Condense (HeaderHash blk)
  , Condense (Header blk)
  , Eq (Header blk)
  , GetHeader blk
  , HasHeader blk
  , IssueTestBlock blk
  , Ord blk
  ) => TestSuite blk TestKey
testSuite = mkTestSuite $ \case
  Genesis k -> at Genesis.testSuite k
  PeerSimulator k -> at PeerSimulator.testSuite k
