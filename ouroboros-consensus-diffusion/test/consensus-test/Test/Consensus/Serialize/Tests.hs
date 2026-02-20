{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Consensus.Serialize.Tests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Proxy (Proxy (..))
import           Ouroboros.Network.Block (HasHeader, StandardHash)
import           Test.Consensus.BlockTree
import           Test.Consensus.Genesis.Setup.GenChains (GenesisTest (..),
                     IssueTestBlock (..), genChains)
import           Test.Consensus.Genesis.ShrinkIndex
import qualified Test.Consensus.PointSchedule as Schedule
import           Test.Consensus.Serialize
import qualified Test.QuickCheck as QC
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.TestBlock (TestBlock, TestHash, testHashFromList)
import           Text.Read (readEither)



-- TODO: Currently using `()` as a dummy key type; this should be replaced
-- with a proper key type.
tests :: TestTree
tests = testGroup "JSON Serialization"
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
  , testGroup "fromReifiedBlockTree . toReifiedBlockTree == id"
    [ testProperty "BlockTree TestBlock" $
      QC.forAll (genTestBlockTree (pure 1))
        (prop_reified_block_tree_conversion (Proxy @TestBlock))
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
  rtcSeed <- fmap Seed QC.arbitrary
  pure ReifiedTestCase {..}

genTestBlockTreeAndPointSchedule
  :: QC.Gen Word -> QC.Gen (ReifiedBlockTree BlockRep, Schedule.PointSchedule BlockRep)
genTestBlockTreeAndPointSchedule branchFactor = do
  -- Create a block tree with @1@ alternative chain.
  blockTree <- genTestBlockTree (pure 1)
  -- Create a 'longRangeAttack' schedule based on the generated chains.
  ps <- Schedule.stToGen (Schedule.longRangeAttack blockTree)
  reifiedBlockTree <- fmap toReifiedBlockTree $ genTestBlockTree branchFactor
  pure (reifiedBlockTree, fmap getBlockRep ps)

genTestBlockTree :: QC.Gen Word -> QC.Gen (BlockTree TestBlock)
genTestBlockTree = fmap gtBlockTree . genChains

genTestHash :: QC.Gen TestHash
genTestHash = fmap (testHashFromList . QC.getNonEmpty) QC.arbitrary

-- | deserialize . serialize == id
--
-- This property asserts that values survive after being serialized
-- and then deserialized.
prop_serialize_inverse
  :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a, Show a, Eq a)
  => Proxy a -> a -> QC.Property
prop_serialize_inverse _ value =
  let
    json1 = Aeson.toJSON value
    cannotParseMsg err = mconcat
      [ "Unable to parse JSON:\n"
      , "JSON: " , show json1 , "\n"
      , "Error: " , err
      ]
    valueNotStableMsg deserializedValue = mconcat
      [ "Value not stable after round-trip:\n"
      , "Original: " , show value , "\n"
      , "After:    " , show deserializedValue
      ]
  in case Aeson.parseEither Aeson.parseJSON json1 of
    Left err -> QC.counterexample (cannotParseMsg err) False
    Right value' -> case value == value' of
      False -> QC.counterexample (valueNotStableMsg value') False
      True  -> QC.property True

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
  let
    json1 = Aeson.toJSON value
    cannotParseMsg err = mconcat
      [ "Unable to parse JSON:\n"
      , "JSON: " , show json1 , "\n"
      , "Error: " , err
      ]
    jsonNotStableMsg reserializedValue = mconcat
      [ "JSON not stable after round-trip:\n"
      , "Original: " , show json1 , "\n"
      , "After:    " , show reserializedValue
      ]
  in case Aeson.parseEither Aeson.parseJSON json1 of
    Left err -> QC.counterexample (cannotParseMsg err) False
    Right value' ->
      let json2 = Aeson.toJSON (value' :: a)
      in case json1 == json2 of
        False -> QC.counterexample (jsonNotStableMsg json2) False
        True  -> QC.property True

-- | read . show == id
--
-- This property asserts that values survive after being converted to a string
-- and then read back.
prop_read_show_inverse
  :: forall a. (Read a, Show a, Eq a)
  => Proxy a -> a -> QC.Property
prop_read_show_inverse _ value =
  let
    str = show value
    cannotParseMsg err = mconcat
      [ "Unable to read string:\n"
      , "String: " , str, "\n"
      , "Error: " , err
      ]
    valueNotStableMsg parsedValue = mconcat
      [ "Value not stable after round-trip:\n"
      , "Original: " , show value , "\n"
      , "After:    " , show parsedValue
      ]
  in case readEither str of
    Left err -> QC.counterexample (cannotParseMsg err) False
    Right value' -> case value == value' of
      False -> QC.counterexample (valueNotStableMsg value') False
      True  -> QC.property True

-- | fromReifiedBlockTree . toReifiedBlockTree == id
prop_reified_block_tree_conversion
  :: forall blk. (Show blk, Eq blk, StandardHash blk, HasHeader blk, IssueTestBlock blk)
  => Proxy blk -> BlockTree blk -> QC.Property
prop_reified_block_tree_conversion proxy blockTree =
  let
    blockTreeEq
      :: BlockTree blk -> BlockTree blk -> QC.Property
    blockTreeEq (BlockTree trunk1 branches1) (BlockTree trunk2 branches2) =
      QC.conjoin
        [ QC.property (trunk1 == trunk1)
        , QC.property (branches1 == branches1)
        ]
    cannotConvertMsg err = mconcat
      [ "Unable to convert from ReifiedBlockTree to BlockTree:\n"
      , "BlockTree: ", show blockTree, "\n"
      , "Error: ", err
      ]
  in case fromReifiedBlockTree proxy (toReifiedBlockTree blockTree) of
      Left err         -> QC.counterexample (cannotConvertMsg err) False
      Right blockTree' -> QC.property True -- blockTreeEq blockTree blockTree'
