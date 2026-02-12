{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.Consensus.Serialize (
    ReifiedTestCase(..)
  , BlockRep(..)
  , getBlockRep
  , BlockRepData(..)
  , ReifiedBlockTree(..)
  , buildReifiedBlockTree
  , mkReifiedTestCase
  , serializeReifiedTestCase
  , deserializeReifiedTestCase
  , TestVersion(..)
  , FormatVersion(..)
) where

import           Cardano.Slotting.Slot (SlotNo(..))
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=), (.:), (.:?), (.!=))
import qualified Data.Aeson.Types as Aeson
import           Data.Proxy
import qualified Data.Text as T
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.Block as AF
import           Test.Consensus.BlockTree
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random
import           Text.Read



-- | A fully concrete consensus test case, suitable for serialization.
--
-- It looks like this type is parameterized over the block type (and it is),
-- but this is only for the sake of the functor instance. We will only ever
-- serialize block summaries ('BlockRep') since that is all the consensus
-- tests need.
data ReifiedTestCase key u = ReifiedTestCase
  -- ^ A key used by the test runner to identify a test or group of tests.
  { rtcTestKey :: key

  -- ^ Since serialized tests can exist beyond a single run, and tests can
  -- change over time, we need a way for the test case to specify which version
  -- of the test it was generated for.
  , rtcTestVersion :: TestVersion

  -- ^ The block tree is represented as a trunk and a list of branches,
  -- oldest nodes first.
  , rtcBlockTree :: ReifiedBlockTree u

  , rtcPointSchedule :: PointSchedule u

  -- ^ Used for specifying a shrink of the generated test case.
  , rtcShrinkIndex :: [Int]

  -- ^ Used for replaying tests.
  , rtcSeed :: QCGen
  } deriving (Show, Functor, Foldable, Traversable)



-- | Representation of a block within a block tree. Since consensus tests do
-- not care about the contents of blocks, we only need enough information to
-- reconstruct the block tree using the methods in `IssueTestBlock` (and thus
-- do not otherwise care about the specific block type).
data BlockRepData = BlockRepData
  { brSlotNum :: SlotNo
  , brHash    :: String
  } deriving (Eq, Show)

instance (Aeson.ToJSON BlockRepData) where
  toJSON BlockRepData{ brSlotNum, brHash } = Aeson.object
    [ "slotNum" .= brSlotNum
    , "hash" .= brHash
    ]

instance (Aeson.FromJSON BlockRepData) where
  parseJSON = Aeson.withObject "BlockRepData" $ \v -> BlockRepData
    <$> v .: "slotNum"
    <*> v .: "hash"

data BlockRep = BlockRep BlockRepData | GenesisBlockRep
  deriving (Eq, Show)

instance (Aeson.ToJSON BlockRep) where
  toJSON rep = case rep of
    GenesisBlockRep -> Aeson.object
      [ "genesis" .= True ]
    BlockRep blockRepData ->
      Aeson.toJSON blockRepData

instance (Aeson.FromJSON BlockRep) where
  parseJSON = Aeson.withObject "BlockRep" $ \v -> do
    isGenesis <- v .:? "genesis" .!= False
    if isGenesis
      then pure GenesisBlockRep
      else BlockRep <$> Aeson.parseJSON (Aeson.Object v)

getBlockRep :: (AF.HasHeader blk) => blk -> BlockRep
getBlockRep blk =
  let headers = AF.getHeaderFields blk
  in BlockRep $ BlockRepData (AF.headerFieldSlot headers) (show $ AF.headerFieldHash headers)

getAnchorRep :: (Show (AF.HeaderHash blk)) => AF.AnchoredFragment blk -> BlockRep
getAnchorRep fragment = case AF.anchor fragment of
  AF.AnchorGenesis -> GenesisBlockRep
  AF.Anchor slot hash _ -> BlockRep $ BlockRepData slot (show hash)

anchoredFragmentToBlockRepOldestFirst
  :: (AF.HasHeader blk)
  => AF.AnchoredFragment blk -> [BlockRep]
anchoredFragmentToBlockRepOldestFirst fragment =
  getAnchorRep fragment : fmap getBlockRep (AF.toOldestFirst fragment)

-- | Representation of the trunk and branches of a block tree as lists, oldest
-- nodes first. Meant to be as normalized as possible and efficient to convert
-- in both directions.
data ReifiedBlockTree u = ReifiedBlockTree
  { rbtTrunk :: [u]
  , rbtBranches :: [[u]]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Aeson.ToJSON u) => Aeson.ToJSON (ReifiedBlockTree u) where
  toJSON ReifiedBlockTree{rbtTrunk, rbtBranches} = Aeson.object
    [ "trunk" .= rbtTrunk
    , "branches" .= rbtBranches
    ]

instance (Aeson.FromJSON u) => Aeson.FromJSON (ReifiedBlockTree u) where
  parseJSON = Aeson.withObject "ReifiedBlockTree" $ \v -> ReifiedBlockTree
    <$> v .: "trunk"
    <*> v .: "branches"

buildReifiedBlockTree
  :: (AF.HasHeader blk) => BlockTree blk -> ReifiedBlockTree BlockRep
buildReifiedBlockTree (BlockTree trunk branches) = ReifiedBlockTree
  (anchoredFragmentToBlockRepOldestFirst trunk)
  (fmap (anchoredFragmentToBlockRepOldestFirst . btbSuffix) branches)



mkReifiedTestCase
  :: (AF.HasHeader blk)
  => TestVersion -> key -> [Int] -> QCGen
  -> BlockTree blk -> PointSchedule blk
  -> ReifiedTestCase key BlockRep
mkReifiedTestCase testVersion key shrinkIndex seed blockTree pointSchedule =
  ReifiedTestCase
    { rtcTestKey = key
    , rtcTestVersion = testVersion
    , rtcBlockTree = buildReifiedBlockTree blockTree
    , rtcPointSchedule = fmap getBlockRep pointSchedule
    , rtcShrinkIndex = shrinkIndex
    , rtcSeed = seed
    }



data FormatVersion
  = FormatVersion_0_0
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON FormatVersion where
  toJSON FormatVersion_0_0 = Aeson.String "0.0"

instance Aeson.FromJSON FormatVersion where
  parseJSON = Aeson.withText "FormatVersion" $ \txt -> case txt of
    "0.0" -> pure FormatVersion_0_0
    _ -> fail $ "Unknown format version: " ++ T.unpack txt

newtype TestVersion = TestVersion String
  deriving (Eq, Ord, Show, Aeson.FromJSON, Aeson.ToJSON)

instance QC.Arbitrary TestVersion where
  arbitrary = TestVersion <$> QC.oneof (fmap pure ["v1", "v2", "v3"])
  shrink (TestVersion x) = if x == "v1" then [] else [TestVersion "v1"]



serializeReifiedTestCase
  :: (Aeson.ToJSON key)
  => FormatVersion
  -> ReifiedTestCase key BlockRep
  -> Aeson.Value
serializeReifiedTestCase fmtVersion testCase = Aeson.object
  [ "formatVersion" .= fmtVersion
  , "key" .= Aeson.toJSON (rtcTestKey testCase)
  , "testVersion" .= rtcTestVersion testCase
  , "blockTree" .= rtcBlockTree testCase
  , "pointSchedule" .= rtcPointSchedule testCase
  , "shrinkIndex" .= rtcShrinkIndex testCase
  , "seed" .= show (rtcSeed testCase) -- QCGen implements Read and Show for serialization
  ]

deserializeReifiedTestCase
  :: (Aeson.FromJSON key)
  => Proxy key -> Aeson.Value -> Aeson.Parser (ReifiedTestCase key BlockRep)
deserializeReifiedTestCase _ = Aeson.withObject "ReifiedTestCase" $ \obj -> do
  fmtVersion <- obj .: "formatVersion"
  case fmtVersion of
    FormatVersion_0_0 ->
      -- Currently we only have one format version.
      ReifiedTestCase
        <$> obj .: "key"
        <*> obj .: "testVersion"
        <*> obj .: "blockTree"
        <*> obj .: "pointSchedule"
        <*> obj .: "shrinkIndex"
        <*> Aeson.explicitParseField deserializeQCGen obj "seed"

-- QCGen implements Read and Show for serialization
deserializeQCGen :: Aeson.Value -> Aeson.Parser QCGen
deserializeQCGen = Aeson.withText "seed" $ \txt ->
  case readMaybe $ T.unpack txt of
    Nothing -> fail "unable to parse seed"
    Just gen -> pure gen

instance (Aeson.ToJSON key) => Aeson.ToJSON (ReifiedTestCase key BlockRep) where
  toJSON = serializeReifiedTestCase FormatVersion_0_0

instance (Aeson.FromJSON key) => Aeson.FromJSON (ReifiedTestCase key BlockRep) where
  parseJSON = deserializeReifiedTestCase Proxy
