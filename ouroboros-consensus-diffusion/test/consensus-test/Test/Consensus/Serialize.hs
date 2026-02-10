{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Consensus.Serialize (
    ReifiedTestCase(..)
  , serializeTestCase
  , deserializeTestCase
  , BlockType(..)
  , TestVersion(..)
  , FormatVersion(..)
  , SomeBlockTreeAndPointSchedule(..)
) where

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
import           Data.Proxy
import qualified Data.Text as T
import           Test.Consensus.BlockTree
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random
import           Test.Util.TestBlock (TestBlock)
import           Text.Read



-- | A fully concrete consensus test case, suitable for serialization.
data ReifiedTestCase key = ReifiedTestCase
  -- ^ A key used by the test runner to identify a test or group of tests.
  { rtcTestKey :: key

  -- ^ Since serialized tests can exist beyond a single run, and tests can
  -- change over time, we need a way to let the test case specify which version
  -- of the test it was generated for.
  , rtcTestVersion :: TestVersion

  -- ^ Although the tests are block polymorphic, to actually run them the block
  -- tree and point schedule have to have a concrete block parameter. We've
  -- hidden these behind a constructor that acts a little like an existential.
  , rtcBlockTreeAndPointSchedule :: SomeBlockTreeAndPointSchedule

  -- ^ Used for specifying a shrink of the generated test case.
  , rtcShrinkIndex :: [Int]

  -- ^ Used for replaying tests.
  , rtcSeed :: QCGen
  }

newtype FormatVersion = FormatVersion String
  deriving (Eq, Ord, Show, Aeson.FromJSON, Aeson.ToJSON)

newtype TestVersion = TestVersion String
  deriving (Eq, Ord, Show, Aeson.FromJSON, Aeson.ToJSON)

instance QC.Arbitrary TestVersion where
  arbitrary = TestVersion <$> QC.oneof (fmap pure ["v1", "v2", "v3"])
  shrink (TestVersion x) = if x == "v1" then [] else [TestVersion "v1"]

data BlockType
  = TestBlockType
  deriving (Eq, Show)

-- | The PointSchedule type is parameterized over a block type.
-- Which is good! Although it makes deserialization a little bit awkward,
-- since the type to parse into depends on the block type. Realistically
-- this tool will be used with a pretty small number of different block
-- types; we wrap them into a type to hide the `blk` parameter.
data SomeBlockTreeAndPointSchedule
  = TestBlockTreeAndPointSchedule (BlockTree TestBlock) (PointSchedule TestBlock)

serializeTestCase
  :: (Aeson.ToJSON key) => FormatVersion -> ReifiedTestCase key
  -> Either T.Text Aeson.Value
serializeTestCase fmtVersion testCase =
  Right $ Aeson.object
    [ "formatVersion" .= fmtVersion
    , "key" .= Aeson.toJSON (rtcTestKey testCase)
    , "testVersion" .= rtcTestVersion testCase
    , "testData" .= serializeSomeBlockTreeAndPointSchedule
        (rtcBlockTreeAndPointSchedule testCase)
    , "shrinkIndex" .= rtcShrinkIndex testCase
    , "seed" .= serializeQCGen (rtcSeed testCase)
    ]

-- QCGen implements Read and Show for serialization
serializeQCGen :: QCGen -> String
serializeQCGen = show

deserializeQCGen :: Aeson.Value -> Aeson.Parser QCGen
deserializeQCGen = Aeson.withText "seed" $ \txt ->
  case readMaybe $ T.unpack txt of
    Nothing -> fail "unable to parse seed"
    Just gen -> pure gen

serializeSomeBlockTreeAndPointSchedule
  :: SomeBlockTreeAndPointSchedule -> Aeson.Value
serializeSomeBlockTreeAndPointSchedule x = case x of
  TestBlockTreeAndPointSchedule blockTree pointSchedule -> Aeson.object
    [ "blockType" .= ("test" :: T.Text)
    , "blockTree" .= Aeson.toJSON blockTree
    , "pointSchedule" .= Aeson.toJSON pointSchedule
    ]

deserializeTestCase
  :: (Aeson.FromJSON key)
  => Proxy key -> Aeson.Value -> Aeson.Parser (ReifiedTestCase key)
deserializeTestCase _ = Aeson.withObject "ReifiedTestCase" $ \obj -> ReifiedTestCase
  <$> obj .: "key"
  <*> obj .: "testVersion"
  <*> (parseBlockTreeAndPointSchedule =<< (obj .: "testData"))
  <*> obj .: "shrinkIndex"
  <*> Aeson.explicitParseField deserializeQCGen obj "seed"

-- Cannot use (just) the applicative interface here because which
-- type gets parsed depends on the value of the 'block_type' field.
parseBlockTreeAndPointSchedule
  :: Aeson.Value -> Aeson.Parser SomeBlockTreeAndPointSchedule
parseBlockTreeAndPointSchedule =
  Aeson.withObject "SomeBlockTreeAndPointSchedule" $ \obj -> do
    rawBlockType <- obj .: "blockType"
    case parseBlockType rawBlockType of
      Left msg -> Aeson.parseFail msg
      Right blockType -> case blockType of
        TestBlockType -> TestBlockTreeAndPointSchedule
          <$> obj .: "blockTree"
          <*> obj .: "pointSchedule"

parseBlockType :: T.Text -> Either String BlockType
parseBlockType txt = case txt of
  "test" -> Right TestBlockType
  _ -> Left $ "invalid block type: " <> T.unpack txt

















