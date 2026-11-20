{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Consensus.Serialize (
    -- * JSON serialization for test cases
    -- $intro
    ReifiedTestCase (..)
  , TestVersion (..)
  , genReifiedTestCase
  , serializeReifiedTestCase
    -- * Helper types
    -- $updateFormatVersion
  , FormatVersion (..)
  , Seed (..)
  ) where

import           Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import           Test.Consensus.Genesis.Setup (TestVersion (..))
import           Test.Consensus.Genesis.ShrinkIndex (ShrinkIndex, path)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random
import           Text.Read (readMaybe)

-- $intro
--
-- This module implements JSON serialization for consensus test cases. Every
-- @ConformanceTest@ has a canonical test case generator and shrinker, so we can
-- get away with representing a test case as a seed for running @Gen@ computations
-- and a path into the shrink tree. We also include a test key to identify which
-- consensus test the data corresponds to, and a test version to allow for backward
-- compatible updates to the generators, shrinkers, and properties.

-- | A uniquely identified consensus test case.
data ReifiedTestCase key = ReifiedTestCase
  { rtcTestKey     :: key
  -- ^ A key used by the test runner to identify a @ConformanceTest@.

  , rtcTestVersion :: TestVersion
  -- ^ Since serialized tests can exist beyond a single run, and tests can
  -- change over time, we need a way for the test case to specify which version
  -- of the test it was generated for.

  , rtcShrinkIndex :: ShrinkIndex
  -- ^ Used for specifying a shrink of the generated test case.

  , rtcSeed        :: Seed
  -- ^ Used for deterministically generating test cases.

  , rtcSize        :: Int
  -- ^ The how big of a test case should we generate?
  } deriving (Eq, Show)

-- | Generate a 'ReifiedTestCase' using the given generator for test keys.
genReifiedTestCase :: QC.Gen key -> QC.Gen (ReifiedTestCase key)
genReifiedTestCase genKey = do
  rtcTestKey <- genKey
  rtcTestVersion <- QC.arbitrary
  rtcShrinkIndex <- fmap (path . fmap QC.getNonNegative) QC.arbitrary
  rtcSeed <- fmap Seed QC.arbitrary
  rtcSize <- fmap QC.getNonNegative QC.arbitrary
  pure ReifiedTestCase
    { rtcTestKey
    , rtcTestVersion
    , rtcShrinkIndex
    , rtcSeed
    , rtcSize
    }

-- | Serialize a 'ReifiedTestCase' using the given 'FormatVersion'.
--
-- See NOTE [UpdatingFormatVersion].
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
  , "size" .= rtcSize testCase
  ]

instance (Aeson.ToJSON key) => Aeson.ToJSON (ReifiedTestCase key) where
  -- See NOTE [UpdatingFormatVersion].
  toJSON = serializeReifiedTestCase FormatVersionOne

instance (Aeson.FromJSON key) => Aeson.FromJSON (ReifiedTestCase key) where
  parseJSON = Aeson.withObject "ReifiedTestCase" $ \obj -> do
    fmtVersion <- obj .: "formatVersion"
    case fmtVersion of
      FormatVersionOne -> do
        -- Currently we only have one format version.
        -- See NOTE [UpdatingFormatVersion].
        rtcTestKey <- obj .: "key"
        rtcTestVersion <- obj .: "testVersion"
        rtcShrinkIndex <- obj .: "shrinkIndex"
        rtcSeed <- obj .: "seed"
        -- The size parameter was originally missing when we first landed this
        -- code. Adding it is thus technically a breaking change, but nobody
        -- actually uses this code yet. So in @FormatVersionOne@ we default it
        -- to 100 if it's missing. Which it probably never will be, but it's a
        -- good default (the same as QuickCheck's default) and its better to be
        -- safe than sorry!
        rtcSize <- obj .:? "size" .!= 100
        pure ReifiedTestCase
          { rtcTestKey
          , rtcTestVersion
          , rtcShrinkIndex
          , rtcSeed
          , rtcSize
          }


{-------------------------------------------------------------------------------
  Helper Types
-------------------------------------------------------------------------------}

newtype Seed = Seed QCGen
  deriving (Show)

-- | 'QCGen' does not have an 'Eq' instance, but
-- 'Read'/'Show' is a canonical serialization.
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
--
-- See NOTE [UpdatingFormatVersion].
data FormatVersion = FormatVersionOne
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON FormatVersion where
  toJSON FormatVersionOne = Aeson.String "v1"

instance Aeson.FromJSON FormatVersion where
  parseJSON = Aeson.withText "FormatVersion" $ \txt ->
    case txt of
      "v1" -> pure FormatVersionOne
      _    -> fail $ "Invalid FormatVersion: " <> T.unpack txt

-- $updateFormatVersion
--
-- NOTE [UpdatingFormatVersion]
--
-- When the JSON format of 'ReifiedTestCase' changes, a new constructor must be
-- added to 'FormatVersion', and the following places must be updated together:
--
--   * 'FormatVersion' and its 'Aeson.ToJSON' / 'Aeson.FromJSON' instances.
--   * 'serializeReifiedTestCase', to emit the new format.
--   * The 'Aeson.ToJSON' instance of 'ReifiedTestCase', to use the new default
--     format version.
--   * The 'Aeson.FromJSON' instance of 'ReifiedTestCase', which must keep
--     parsing all previous format versions for backward compatibility.
--   * The tests in "Test.Consensus.Serialize.Tests", which hardcode the
--     expected format version and top-level keys.
--
-- This is not enforced by the type checker, so take care to keep these in sync.
