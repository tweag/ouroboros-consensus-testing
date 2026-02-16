{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Consensus.Serialize.Tests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Proxy (Proxy(..))
import qualified Test.QuickCheck as QC
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Test.Consensus.BlockTree
import           Test.Consensus.Genesis.Setup.GenChains (GenesisTest(..), genChains)
import qualified Test.Consensus.PointSchedule as Schedule
import           Test.Consensus.Serialize
import           Test.Util.TestBlock (TestBlock)



tests :: TestTree
tests =
  testGroup "JSON Serialization"
    [ testGroup "serialize . deserialize . serialize == serialize"
      [ testProperty "ReifiedTestCase () BlockRep" $
        QC.forAll (genReifiedTestCase (pure 1))
          (prop_roundtrip_serialize (Proxy @(ReifiedTestCase () BlockRep)))
      ]
    ]

genReifiedTestCase
  :: (QC.Arbitrary key)
  => QC.Gen Word -> QC.Gen (ReifiedTestCase key BlockRep)
genReifiedTestCase branchFactor = do
  (blockTree, pointSchedule) <- genTestBlockTreeAndPointSchedule branchFactor
  ReifiedTestCase
    <$> QC.arbitrary
    <*> QC.arbitrary
    <*> pure blockTree
    <*> pure pointSchedule
    <*> QC.arbitrary
    <*> QC.arbitrary

genTestBlockTreeAndPointSchedule
  :: QC.Gen Word -> QC.Gen (ReifiedBlockTree BlockRep, Schedule.PointSchedule BlockRep)
genTestBlockTreeAndPointSchedule branchFactor = do
  -- Create a block tree with @1@ alternative chain.
  blockTree <- genTestBlockTree (pure 1)
  -- Create a 'longRangeAttack' schedule based on the generated chains.
  ps <- Schedule.stToGen (Schedule.longRangeAttack blockTree)
  reifiedBlockTree <- buildReifiedBlockTree <$> genTestBlockTree branchFactor
  (,) <$> pure reifiedBlockTree <*> pure (fmap getBlockRep ps)

genTestBlockTree :: QC.Gen Word -> QC.Gen (BlockTree TestBlock)
genTestBlockTree = fmap gtBlockTree . genChains



-- | serialize . deserialize . serialize == serialize
--
-- This property tests that after deserializing and then serializing again,
-- the JSON is the same as the original. We test this by comparing JSON values
-- rather than the Haskell values because the data types don't have Eq instances.
prop_roundtrip_serialize
  :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a)
  => Proxy a -> a -> QC.Property
prop_roundtrip_serialize _ value =
  case runRoundtrip of
    Left err -> QC.counterexample err False
    Right () -> QC.property True
  where
    runRoundtrip :: Either String ()
    runRoundtrip = do
      let json1 = Aeson.toJSON value
      value' <- Aeson.parseEither Aeson.parseJSON json1
      let json2 = Aeson.toJSON (value' :: a)
      if json1 == json2
        then Right ()
        else Left $
          "JSON not stable after round-trip:\n" ++
          "Original: " ++ show json1 ++ "\n" ++
          "After:    " ++ show json2
