{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Genesis.Tests (
    GenesisTestKey
  , testSuite
  , tests
  ) where

import qualified Data.Aeson as Aeson
import           Ouroboros.Consensus.Block.Abstract (GetHeader, HasHeader,
                     Header)
import           Ouroboros.Consensus.Util.Condense (Condense)
import           Test.Consensus.Genesis.Setup
import qualified Test.Consensus.Genesis.Tests.CSJ as CSJ
import qualified Test.Consensus.Genesis.Tests.DensityDisconnect as GDD
import qualified Test.Consensus.Genesis.Tests.LoE as LoE
import qualified Test.Consensus.Genesis.Tests.LongRangeAttack as LongRangeAttack
import qualified Test.Consensus.Genesis.Tests.LoP as LoP
import qualified Test.Consensus.Genesis.Tests.Uniform as Uniform
import           Test.Consensus.Genesis.TestSuite
import           Test.Tasty
import           Test.Util.TestBlock (TestBlock)

tests :: TestTree
tests = testGroup "Genesis tests" $
  [GDD.tests] <> toTestTree @TestBlock testSuite

-- | Each value of this type uniquely corresponds to a Genesis test.
data GenesisTestKey = Uniform !Uniform.TestKey
             | CSJ !CSJ.TestKey
             | GDD !GDD.TestKey
             | LongRangeAttack !LongRangeAttack.TestKey
             | LoE !LoE.TestKey
             | LoP !LoP.TestKey
  deriving stock (Eq, Ord, Generic)
  deriving SmallKey via Generically GenesisTestKey

instance Aeson.ToJSON GenesisTestKey where
  toJSON = error "ToJSON GenesisTestKey: not yet implemented"
    -- TODO(nbloomf): fix this once test keys are implemented
    -- Aeson.toJSON . keyName

instance Aeson.FromJSON GenesisTestKey where
  parseJSON = Aeson.withText "GenesisTestKey" $ \_ ->
    error "FromJSON GenesisTestKey: not yet implemented"
    -- TODO(nbloomf): fix this once test keys are implemented
    -- case parseKeyName $ T.unpack t of
    --  Just k -> pure k
    --  Nothing -> fail $ "Unrecognized key name: " <> show t

testSuite ::
  ( HasHeader blk
  , GetHeader blk
  , IssueTestBlock blk
  , Condense (Header blk)
  , Ord blk
  , Eq (Header blk)
  ) => TestSuite blk GenesisTestKey
testSuite = mkTestSuite $ \case
  Uniform t -> at Uniform.testSuite t
  CSJ t -> at CSJ.testSuite t
  GDD t -> at GDD.testSuite t
  LongRangeAttack t -> at LongRangeAttack.testSuite t
  LoE t -> at LoE.testSuite t
  LoP t -> at LoP.testSuite t
