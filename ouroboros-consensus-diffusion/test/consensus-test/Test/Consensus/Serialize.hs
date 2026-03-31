{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Consensus.Serialize (
    -- * JSON serialization for test cases
    -- $intro
    FormatVersion (..)
  , ReifiedTestCase (..)
  , Seed (..)
  , TestVersion (..)
  , serializeReifiedTestCase
  ) where

import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import           Test.Consensus.Genesis.ShrinkIndex (ShrinkIndex, path)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random
import           Text.Read (readMaybe)

-- $intro
--
-- This module implements JSON serialization for consensus test cases. Every
-- @ConsensusTest@ has a canonical test case generator and shrinker, so we can
-- get away with representing a test case as a seed for running @Gen@ computations
-- and a path into the shrink tree. We also include a test key to identify which
-- consensus test the data corresponds to, and a test version to allow for backward
-- compatible updates to the generators, shrinkers, and properties.

-- | A uniquely identified consensus test case.
data ReifiedTestCase key = ReifiedTestCase
  { rtcTestKey     :: key
  -- ^ A key used by the test runner to identify a @ConsensusTest@.

  , rtcTestVersion :: TestVersion
  -- ^ Since serialized tests can exist beyond a single run, and tests can
  -- change over time, we need a way for the test case to specify which version
  -- of the test it was generated for.

  , rtcShrinkIndex :: ShrinkIndex
  -- ^ Used for specifying a shrink of the generated test case.

  , rtcSeed        :: Seed
  -- ^ Used for deterministically generating test cases.
  } deriving (Eq, Show)

instance (QC.Arbitrary key) => QC.Arbitrary (ReifiedTestCase key) where
  arbitrary = do
    rtcTestKey <- QC.arbitrary
    rtcTestVersion <- QC.arbitrary
    rtcShrinkIndex <- fmap (path . fmap QC.getNonNegative) QC.arbitrary
    rtcSeed <- fmap Seed QC.arbitrary
    pure ReifiedTestCase {..}

serializeReifiedTestCase
  :: (Aeson.ToJSON key)
  => FormatVersion
  -> ReifiedTestCase key
  -> Aeson.Value
serializeReifiedTestCase fmtVersion testCase = Aeson.object
  [ "formatVersion" .= fmtVersion
  , "key" .= Aeson.toJSON (rtcTestKey testCase)
  , "testVersion" .= rtcTestVersion testCase
  , "shrinkIndex" .= rtcShrinkIndex testCase
  , "seed" .= rtcSeed testCase
  ]

instance (Aeson.ToJSON key) => Aeson.ToJSON (ReifiedTestCase key) where
  -- If the default format version changes, update this.
  toJSON = serializeReifiedTestCase FormatVersionOne

instance (Aeson.FromJSON key) => Aeson.FromJSON (ReifiedTestCase key) where
  parseJSON = Aeson.withObject "ReifiedTestCase" $ \obj -> do
    fmtVersion <- obj .: "formatVersion"
    case fmtVersion of
      FormatVersionOne -> do
        -- Currently we only have one format version.
        rtcTestKey <- obj .: "key"
        rtcTestVersion <- obj .: "testVersion"
        rtcShrinkIndex <- obj .: "shrinkIndex"
        rtcSeed <- obj .: "seed"
        pure ReifiedTestCase {..}



-- Helper Types --
------------------

newtype Seed = Seed QCGen
  deriving (Show)

-- QCGen does not have an Eq instance, but
-- Read/Show is a canonical serialization.
instance Eq Seed where
  Seed a == Seed b = show a == show b

instance Aeson.ToJSON Seed where
  toJSON (Seed gen) = Aeson.String (T.pack $ show gen)

instance Aeson.FromJSON Seed where
  parseJSON = Aeson.withText "Seed" $ \txt ->
    case readMaybe (T.unpack txt) of
      Just gen -> pure $ Seed gen
      Nothing  -> fail "unable to parse seed"

-- | A version number for the serialization format. This is included to
-- allow for backward compatibility in case the JSON format needs to change.
-- This only exists in the JSON, and consumers of this library should not
-- use or rely on it.
data FormatVersion = FormatVersionOne
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON FormatVersion where
  toJSON FormatVersionOne = Aeson.String "v1"

instance Aeson.FromJSON FormatVersion where
  parseJSON = Aeson.withText "FormatVersion" $ \txt ->
    case txt of
      "v1" -> pure FormatVersionOne
      _    -> fail $ "Invalid FormatVersion: " <> T.unpack txt

-- | A version number for the property test itself (as represented by 'key').
-- This is included to allow for backward compatibility in case the property
-- needs to change.
newtype TestVersion = TestVersion Int
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON TestVersion where
  toJSON (TestVersion v) = Aeson.Number (fromIntegral v)

instance Aeson.FromJSON TestVersion where
  parseJSON = Aeson.withScientific "TestVersion" $ \sci ->
    case toBoundedInteger sci of
      Just v  -> pure (TestVersion v)
      Nothing -> fail $ "Invalid TestVersion: " <> show sci

instance QC.Arbitrary TestVersion where
  arbitrary = do
    QC.NonNegative m <- QC.arbitrary
    pure $ TestVersion m
  shrink (TestVersion x) = do
    QC.NonNegative m <- QC.shrink (QC.NonNegative x)
    pure $ TestVersion m
