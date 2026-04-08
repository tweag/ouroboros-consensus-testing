{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Serialize.Tests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as Aeson
import           Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import qualified Test.Consensus.Genesis.TestSuite.All as All
import           Test.Consensus.Genesis.TestSuite.SmallKey
import           Test.Consensus.Serialize
import qualified Test.QuickCheck as QC
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)



tests :: TestTree
tests = testGroup "JSON Serialization"
  [ testGroup "ReifiedTestCase ()"
    [ testProperty "toJSON . fromJSON . toJSON == toJSON" $
      QC.forAll QC.arbitrary
        (prop_serialize_weak_inverse (Proxy @(ReifiedTestCase All.TestKey)))
    , testProperty "fromJSON . toJSON == id" $
      QC.forAll QC.arbitrary
        (prop_deserialize_inverse (Proxy @(ReifiedTestCase All.TestKey)))
    , testProperty "serializeReifiedTestCase emits exactly expected top-level keys" $
      QC.forAll QC.arbitrary $
        prop_serializeReifiedTestCase_emits_expected_keys @All.TestKey
    , testProperty "deserializeReifiedTestCase rejects missing required fields" $
      QC.forAll QC.arbitrary $
        prop_deserializeReifiedTestCase_rejects_missing_required_field @All.TestKey
    , testProperty "deserializeReifiedTestCase rejects invalid required field types" $
      QC.forAll QC.arbitrary $
        prop_deserializeReifiedTestCase_rejects_bad_field_type @All.TestKey
    , testProperty "deserializeReifiedTestCase ignores unknown fields" $
      QC.forAll QC.arbitrary $
        prop_deserializeReifiedTestCase_ignores_unknown_fields @All.TestKey
    ]
   , testGroup "TestKey" $
    [ testProperty "toJSON . fromJSON . toJSON == toJSON" $
      QC.forAll (genKey)
        (prop_serialize_weak_inverse (Proxy @All.TestKey))
    , testProperty "fromJSON . toJSON == id" $
      QC.forAll (genKey)
        (prop_deserialize_inverse (Proxy @All.TestKey))
    ]
  ]



-- Generators --
----------------

genKey :: (SmallKey k) => QC.Gen k
genKey = QC.elements getAllKeys

instance QC.Arbitrary (All.TestKey) where
  arbitrary = genKey



-- Properties --
----------------

-- | fromJSON . toJSON == id
--
-- This property asserts that values survive after being serialized
-- and then deserialized.
prop_deserialize_inverse
  :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a, Show a, Eq a)
  => Proxy a -> a -> QC.Property
prop_deserialize_inverse _ value =
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

-- | toJSON . fromJSON . toJSON == toJSON
--
-- This property tests that if a JSON value was produced by serializing a reified
-- test case, then deserializing and serializing again gives the same JSON value.
-- This is weaker than saying that fromJSON . toJSON == id, but if that
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



-- | Serialized test cases have the expected keys.
prop_serializeReifiedTestCase_emits_expected_keys
  :: forall key. (Aeson.ToJSON key, Aeson.FromJSON key)
  => ReifiedTestCase key -> QC.Property
prop_serializeReifiedTestCase_emits_expected_keys reified =
  case serializeReifiedTestCase FormatVersionOne reified of
    Aeson.Object obj ->
      let
        observed = Set.fromList (fmap AesonKey.toText $ AesonKeyMap.keys obj)
        expected = Set.fromList
          [ "formatVersion"
          , "key"
          , "testVersion"
          , "shrinkIndex"
          , "seed"
          ]
        msg = mconcat
          [ "Unexpected top-level JSON keys in serializeReifiedTestCase output.\n"
          , "Expected: ", show expected, "\n"
          , "Observed: ", show observed
          ]
      in QC.counterexample msg $ QC.property (observed == expected)
    value -> QC.counterexample ("Expected object JSON, got: " <> show value) False

-- | Deserializing a ReifiedTestCase with any required field missing should fail.
prop_deserializeReifiedTestCase_rejects_missing_required_field
  :: forall key. (Aeson.FromJSON key, Aeson.ToJSON key, Show key)
  => ReifiedTestCase key -> QC.Property
prop_deserializeReifiedTestCase_rejects_missing_required_field reified =
  case serializeReifiedTestCase FormatVersionOne reified of
    Aeson.Object obj ->
      QC.conjoin $ fmap (fieldMustFailWhenMissing obj) requiredFields
    value -> QC.counterexample ("Expected object JSON, got: " <> show value) False
  where
    requiredFields :: [AesonKey.Key]
    requiredFields =
      [ "formatVersion"
      , "key"
      , "testVersion"
      , "shrinkIndex"
      , "seed"
      ]

    fieldMustFailWhenMissing
      :: Aeson.Object
      -> AesonKey.Key
      -> QC.Property
    fieldMustFailWhenMissing obj field =
      let mutated = Aeson.Object $ AesonKeyMap.delete field obj
      in case parseReifiedTestCaseValue @key mutated of
          Left _ -> QC.property True
          Right parsed -> QC.counterexample
            (mconcat
              [ "Expected parsing to fail when field is missing: "
              , show (AesonKey.toText field)
              , "\nParsed value: "
              , show parsed
              ])
            False

-- | Deserializing a ReifiedTestCase with an invalid type
-- for a required field should fail.
prop_deserializeReifiedTestCase_rejects_bad_field_type
  :: forall key. (Aeson.FromJSON key, Aeson.ToJSON key, Show key)
  => ReifiedTestCase key -> QC.Property
prop_deserializeReifiedTestCase_rejects_bad_field_type reified =
  case serializeReifiedTestCase FormatVersionOne reified of
    Aeson.Object obj ->
      QC.conjoin $ fmap (mutationMustFail obj)
        [ ("formatVersion", Aeson.Bool True)
        , ("testVersion", Aeson.String "0.0")
        , ("shrinkIndex", Aeson.Bool True)
        , ("seed", Aeson.Number 0)
        ]
    value -> QC.counterexample ("Expected object JSON, got: " <> show value) False
  where
    mutationMustFail
      :: Aeson.Object
      -> (AesonKey.Key, Aeson.Value)
      -> QC.Property
    mutationMustFail obj (field, badValue) =
      let mutated = Aeson.Object $ AesonKeyMap.insert field badValue obj
      in case parseReifiedTestCaseValue @key mutated of
          Left _ -> QC.property True
          Right parsed -> QC.counterexample
            (mconcat
              [ "Expected parsing to fail for invalid field type.\n"
              , "Field: ", show (AesonKey.toText field), "\n"
              , "Bad value: ", show badValue, "\n"
              , "Parsed value: ", show parsed
              ])
            False

-- | Adding unknown JSON fields should not cause parsing to fail, and the
-- unknown fields should be ignored.
prop_deserializeReifiedTestCase_ignores_unknown_fields
  :: forall key. (Aeson.FromJSON key, Aeson.ToJSON key, Eq key, Show key)
  => ReifiedTestCase key -> QC.Property
prop_deserializeReifiedTestCase_ignores_unknown_fields reified =
  case serializeReifiedTestCase FormatVersionOne reified of
    Aeson.Object obj ->
      let
        mutated = Aeson.Object $ AesonKeyMap.insert "_unknownField" (Aeson.String "extra") obj
      in case parseReifiedTestCaseValue @key mutated of
          Left err -> QC.counterexample
            ("Expected parse success with unknown field, got error: " <> err)
            False
          Right parsed -> QC.counterexample
            (mconcat
              [ "Unknown fields should be ignored during parsing.\n"
              , "Expected: ", show reified, "\n"
              , "Parsed: ", show parsed
              ])
            (QC.property (parsed == reified))
    value -> QC.counterexample ("Expected object JSON, got: " <> show value) False

parseReifiedTestCaseValue
  :: (Aeson.FromJSON key) => Aeson.Value -> Either String (ReifiedTestCase key)
parseReifiedTestCaseValue = Aeson.parseEither Aeson.parseJSON
