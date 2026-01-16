{-# LANGUAGE OverloadedStrings #-}
module Test.Consensus.Serialize where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
-- import           Test.Consensus.Genesis.TestSuite
import           Test.Consensus.BlockTree
import           Test.Consensus.PointSchedule
import           Test.QuickCheck.Random

newtype FormatVersion = FormatVersion String
  deriving (Eq, Ord, Show)

newtype TestVersion = TestVersion String
  deriving (Eq, Ord, Show)
  
data TestClass -- dummy, only here until this lands:
               -- https://github.com/tweag/ouroboros-consensus-testing/pull/28

-- TODO: From the quickcheck documentation:
--   Note: saving a seed from one version of QuickCheck and replaying it in
--   another is not supported. If you want to store a test case permanently
--   you should save the test case itself.
--
-- If the versions of quickcheck used to generate a test case and later run it
-- are different, then the seed might be meaningless. Probably the easist way
-- to fix this is just say that bumping the version number of the quickcheck
-- dependency requires bumbing the version number on the tests too, but that
-- is hard to enforce. Do we need to keep the quickcheck version number too?

-- | A fully concrete test case, suitable for serialization.
data ReifiedTestCase = ReifiedTestCase
  -- ^ A key used by the test runner to identify a test or group of tests.
  { rtcTestClass   :: TestClass

  -- ^ Since serialized tests can exist beyond a single run, and tests can
  -- change over time, we need a way to let the test case specify which version
  -- of the test it was generated for.
  --
  -- TODO: What exactly does TestClass represent? Is it a single test or multiple?
  -- If multiple then the TestVersion will not be a well-defined concept, since
  -- individual tests would change at different times. We may need something like
  -- [(TestClass, TestVersion)] instead.
  , rtcTestVersion :: TestVersion

  -- ^ Although the tests are block polymorphic, to actually run them the block
  -- tree and point schedule have to have a concrete block parameter. We've
  -- hidden these behind a constructor that acts a little like an existential.
  , rtcBlockTreeAndPointSchedule :: SomeBlockTreeAndPointSchedule

  -- ^ Used for replaying tests.
  , rtcSeed :: QCGen
  
  -- ^ Used for replaying tests. The ShrinkIndex type is defined in a package this
  -- one has no access to (TODO: I think, need to look at this again.)
  , rtcShrinkIndex :: [Int]
  }

data BlockType
  = UnitBlockType
  deriving (Eq, Show)

-- | The PointSchedule type is parameterized over a block type.
-- Which is good! Although it makes deserialization a little bit awkward,
-- since the type to parse into depends on the block type. Realistically
-- this tool will be used with a pretty small number of different block
-- types; we wrap them into a type to hide the `blk` parameter.
data SomeBlockTreeAndPointSchedule
  = UnitBlockTreeAndPointSchedule (BlockTree ()) (PointSchedule ())


data SerializationError = SerializationError
  deriving (Eq, Show)

serializeTestCase :: ReifiedTestCase -> Either SerializationError Aeson.Value
serializeTestCase testCase = Left SerializationError

data DeserializationError = DeserializationError
  deriving (Eq, Show)

deserializeTestCase :: Aeson.Value -> Aeson.Parser ReifiedTestCase
deserializeTestCase value = undefined

-- | Cannot use (just) the applicative interface here because which
-- type gets parsed depends on the value of the 'block_type' field.
parseBlockTreeAndPointSchedule
  :: Aeson.Value -> Aeson.Parser SomeBlockTreeAndPointSchedule
parseBlockTreeAndPointSchedule =
  Aeson.withObject "SomeBlockTreeAndPointSchedule" $ \obj ->
    let
      getBlockType = case Aeson.lookup "block_type" obj of
        Nothing -> Left "block_type key not found."
        Just v -> parseBlockType v
    in case getBlockType of
      Left msg -> Aeson.parseFail msg
      Right blockType -> case blockType of
        UnitBlockType -> UnitBlockTreeAndPointSchedule
          <$> parseUnitBlockTree <*> parseUnitPointSchedule

parseBlockType :: Aeson.Value -> Either String BlockType
parseBlockType = Aeson.withString txt -> case txt of
  "unit" -> UnitBlockType

parseUnitBlockTree :: Aeson.Parser (BlockTree ())
parseUnitBlockTree = undefined

parseUnitPointSchedule :: Aeson.Parser (PointSchedule ())
parseUnitPointSchedule = undefined

-- properties:
--   deserializeTestCase . serializeTestCase === id
--
--   * this direction is more complicated because of the file format version string;
--     serializeTestCase may use a different version than what the test was originally serialized with.
--   ignoreFileFormatVersion . serializeTestCase . deserializeTestCase === ignoreFileFormatVersion
