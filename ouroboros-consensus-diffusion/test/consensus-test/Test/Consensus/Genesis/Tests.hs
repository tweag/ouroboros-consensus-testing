{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Genesis.Tests (
    GenesisTests
  , testSuite
  , tests
  ) where

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

data GenesisTests = Uniform !Uniform.Test
                  | CSJ !CSJ.Test
                  | GDD !GDD.Test
                  | LRA !LongRangeAttack.Test
                  | LoE !LoE.Test
                  | LoP !LoP.Test
                 deriving stock (Eq, Ord, Generic)
                 deriving (Universe, Finite) via GenericUniverse GenesisTests

testSuite ::
  ( HasHeader blk
  , GetHeader blk
  , IssueTestBlock blk
  , Condense (Header blk)
  , Ord blk
  , Eq (Header blk)
  ) => TestSuite blk GenesisTests
testSuite = mkTestSuite $ \case
  Uniform t -> at Uniform.testSuite t
  CSJ t -> at CSJ.testSuite t
  GDD t -> at GDD.testSuite t
  LRA t -> at LongRangeAttack.testSuite t
  LoE t -> at LoE.testSuite t
  LoP t -> at LoP.testSuite t
