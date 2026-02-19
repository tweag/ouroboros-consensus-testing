{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
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
import           Test.Consensus.Genesis.ShrinkIndex
import qualified Test.Consensus.PointSchedule as Schedule
import           Test.Consensus.Serialize
import           Test.Util.TestBlock (TestBlock)



-- TODO: Currently using `()` as a dummy key type; this should be replaced
-- with a proper key type.
tests :: TestTree
tests =
  testGroup "JSON Serialization"
    [ testGroup "serialize . deserialize . serialize == serialize"
      [ testProperty "ReifiedTestCase () BlockRep" $
        QC.forAll (genReifiedTestCase (pure 1))
          (prop_serialize_weak_inverse (Proxy @(ReifiedTestCase () BlockRep)))
      ]
    , testGroup "deserialize . serialize == id"
      [ testProperty "ReifiedTestCase () BlockRep" $
        QC.forAll (genReifiedTestCase (pure 1))
          (prop_serialize_inverse (Proxy @(ReifiedTestCase () BlockRep)))
      ]
    ]

genReifiedTestCase
  :: (QC.Arbitrary key)
  => QC.Gen Word -> QC.Gen (ReifiedTestCase key BlockRep)
genReifiedTestCase branchFactor = do
  (rtcBlockTree, rtcPointSchedule) <- genTestBlockTreeAndPointSchedule branchFactor
  rtcTestKey <- QC.arbitrary
  rtcTestVersion <- QC.arbitrary
  rtcShrinkIndex <- fmap (path . fmap QC.getNonNegative) QC.arbitrary
  rtcSeed <- QC.arbitrary
  pure ReifiedTestCase {..}

genTestBlockTreeAndPointSchedule
  :: QC.Gen Word -> QC.Gen (ReifiedBlockTree BlockRep, Schedule.PointSchedule BlockRep)
genTestBlockTreeAndPointSchedule branchFactor = do
  -- Create a block tree with @1@ alternative chain.
  blockTree <- genTestBlockTree (pure 1)
  -- Create a 'longRangeAttack' schedule based on the generated chains.
  ps <- Schedule.stToGen (Schedule.longRangeAttack blockTree)
  reifiedBlockTree <- toReifiedBlockTree <$> genTestBlockTree branchFactor
  (,) <$> pure reifiedBlockTree <*> pure (fmap getBlockRep ps)

genTestBlockTree :: QC.Gen Word -> QC.Gen (BlockTree TestBlock)
genTestBlockTree = fmap gtBlockTree . genChains

-- | deserialize . serialize == id
--
-- This property asserts that values survive after being serialized
-- and then deserialized.
prop_serialize_inverse
  :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a, Show a, Eq a)
  => Proxy a -> a -> QC.Property
prop_serialize_inverse _ value =
  case runRoundtrip of
    Left err -> QC.counterexample err False
    Right () -> QC.property True
  where
    runRoundtrip :: Either String ()
    runRoundtrip = do
      let json1 = Aeson.toJSON value
      value' <- Aeson.parseEither Aeson.parseJSON json1
      case value == value' of
        True -> Right ()
        False -> Left $
          "Value not stable after round-trip:\n" <>
          "Original: " <> show value <> "\n" <>
          "After:    " <> show value'

-- | serialize . deserialize . serialize == serialize
--
-- This property tests that if a JSON value was produced by serializing a reified
-- test case, then deserializing and serializing again gives the same JSON value.
-- This is weaker than saying that deserialize . serialize == id, but if that
-- test fails, whether or not this one passes can help with debugging.
prop_serialize_weak_inverse
  :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a)
  => Proxy a -> a -> QC.Property
prop_serialize_weak_inverse _ value =
  case runRoundtrip of
    Left err -> QC.counterexample err False
    Right () -> QC.property True
  where
    runRoundtrip :: Either String ()
    runRoundtrip = do
      let json1 = Aeson.toJSON value
      value' <- Aeson.parseEither Aeson.parseJSON json1
      let json2 = Aeson.toJSON (value' :: a)
      case json1 == json2 of
        True -> Right ()
        False -> Left $
          "JSON not stable after round-trip:\n" ++
          "Original: " ++ show json1 ++ "\n" ++
          "After:    " ++ show json2
