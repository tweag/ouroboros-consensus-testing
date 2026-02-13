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
{-# LANGUAGE RecordWildCards #-}
module Test.Consensus.Serialize (
    ReifiedTestCase(..)
  , BlockRep(..)
  , getBlockRep
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
import           Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
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
  { rtcTestKey :: key
  -- ^ A key used by the test runner to identify a test or group of tests.

  , rtcTestVersion :: TestVersion
  -- ^ Since serialized tests can exist beyond a single run, and tests can
  -- change over time, we need a way for the test case to specify which version
  -- of the test it was generated for.

  , rtcBlockTree :: ReifiedBlockTree u
  -- ^ The block tree is represented as a trunk and a list of branches,
  -- oldest nodes first.

  , rtcPointSchedule :: PointSchedule u

  , rtcShrinkIndex :: [Int]
  -- ^ Used for specifying a shrink of the generated test case.

  , rtcSeed :: QCGen
  -- ^ Used for replaying tests.
  } deriving (Show, Functor, Foldable, Traversable)

-- | Representation of a block within a block tree. Since consensus tests do
-- not care about the contents of blocks, we only need enough information to
-- reconstruct the block tree using the methods in `IssueTestBlock` (and thus
-- do not otherwise care about the specific block type).
data BlockRep = BlockRep
  { brSlotNum :: SlotNo
  , brHash    :: T.Text
  } deriving (Eq, Show)

instance (Aeson.ToJSON BlockRep) where
  toJSON BlockRep{ brSlotNum, brHash } = Aeson.object
    [ "slotNum" .= brSlotNum
    , "hash" .= brHash
    ]

instance (Aeson.FromJSON BlockRep) where
  parseJSON = Aeson.withObject "BlockRep" $ \v -> do
    brSlotNum <- v .: "slotNum"
    brHash <- v .: "hash"
    pure BlockRep {..}

getBlockRep :: (AF.HasHeader blk) => blk -> BlockRep
getBlockRep blk =
  let headers = AF.getHeaderFields blk
  in  BlockRep (AF.headerFieldSlot headers) (T.pack $ show $ AF.headerFieldHash headers)

getAnchorRep
  :: (Show (AF.HeaderHash blk)) => AF.AnchoredFragment blk -> Maybe BlockRep
getAnchorRep fragment = case AF.anchor fragment of
  AF.AnchorGenesis -> Nothing
  AF.Anchor slot hash _ -> Just $ BlockRep slot (T.pack $ show hash)

-- | 'AnchoredList' is a simplified representation of an 'AnchoredFragment'
-- as a list; an anchor of 'Nothing' represents the genesis.
data AnchoredList u = AnchoredList
  { alAnchor :: Maybe u
  , alBlocks :: [u]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Aeson.ToJSON u) => Aeson.ToJSON (AnchoredList u) where
  toJSON AnchoredList{alAnchor, alBlocks} = Aeson.object
    [ "anchor" .= case alAnchor of
        Nothing -> Aeson.String "genesis"
        Just rep -> Aeson.toJSON rep
    , "blocks" .= alBlocks
    ]

instance (Aeson.FromJSON u) => Aeson.FromJSON (AnchoredList u) where
  parseJSON = Aeson.withObject "AnchoredList" $ \v -> do
    alAnchor <- do
      val <- v .: "anchor"
      case val of
        Aeson.String "genesis" -> pure Nothing
        _ -> Just <$> Aeson.parseJSON val
    alBlocks <- v .: "blocks"
    pure AnchoredList {..}

-- | Representation of the trunk and branches of a block tree as lists, oldest
-- nodes first. Meant to be as normalized as possible and efficient to convert
-- in both directions. Branches are represented as suffixes off the trunk, where
-- the anchor is a trunk node; this is enough to reconstruct the tree without
-- redundant information.
data ReifiedBlockTree blk = ReifiedBlockTree
  { rbtTrunk :: AnchoredList blk
  , rbtBranches :: [AnchoredList blk]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Aeson.ToJSON blk) => Aeson.ToJSON (ReifiedBlockTree blk) where
  toJSON ReifiedBlockTree{rbtTrunk, rbtBranches} =
    Aeson.object
      [ "trunk" .= rbtTrunk
      , "branches" .= rbtBranches
      ]

instance (Aeson.FromJSON blk) => Aeson.FromJSON (ReifiedBlockTree blk) where
  parseJSON = Aeson.withObject "ReifiedBlockTree" $ \v -> do
    rbtTrunk <- v .: "trunk"
    rbtBranches <- v .: "branches"
    pure ReifiedBlockTree {..}

-- | Represent an 'AnchoredFragment' as a list of 'BlockRep's, from oldest to
-- newest, plus the anchor.
anchoredFragmentToAnchoredListOldestFirst
  :: (AF.HasHeader blk) => AF.AnchoredFragment blk -> AnchoredList BlockRep
anchoredFragmentToAnchoredListOldestFirst fragment = AnchoredList
  (getAnchorRep fragment) (fmap getBlockRep (AF.toOldestFirst fragment))

-- | Summarize a block tree as a 'ReifiedBlockTree'. This is the representation
-- we will serialize.
buildReifiedBlockTree
  :: (AF.HasHeader blk) => BlockTree blk -> ReifiedBlockTree BlockRep
buildReifiedBlockTree (BlockTree trunk branches) = ReifiedBlockTree
  (anchoredFragmentToAnchoredListOldestFirst trunk)
  (fmap (anchoredFragmentToAnchoredListOldestFirst . btbSuffix) branches)

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

-- | A version number for the serialization format. This is included to
-- allow for backward compatibility in case the JSON format needs to change.
-- This only exists in the JSON.
data FormatVersion
  = FormatVersion_0_0
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON FormatVersion where
  toJSON FormatVersion_0_0 = Aeson.String "0.0"

instance Aeson.FromJSON FormatVersion where
  parseJSON = Aeson.withText "FormatVersion" $ \txt -> case txt of
    "0.0" -> pure FormatVersion_0_0
    _ -> fail $ "Unknown format version: " ++ T.unpack txt

-- | A version number for the property test itself (as represented by 'key').
-- This is included to allow for backward compatibility in case the property
-- needs to change.
newtype TestVersion = TestVersion Int
  deriving (Eq, Ord, Show, Aeson.FromJSON, Aeson.ToJSON)

instance QC.Arbitrary TestVersion where
  arbitrary = TestVersion <$> QC.choose (0,5)
  shrink (TestVersion x) =
    let (absx, sgnx) = (abs x, signum x)
    in case compare absx 0 of
      GT -> fmap (TestVersion . (sgnx*)) [0..(absx-1)]
      EQ -> []
      LT -> error "absolute value cannot be negative"

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
  => Aeson.Value -> Aeson.Parser (ReifiedTestCase key BlockRep)
deserializeReifiedTestCase = Aeson.withObject "ReifiedTestCase" $ \obj -> do
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
  parseJSON = deserializeReifiedTestCase

rehydrateTestCase :: ReifiedTestCase key BlockRep -> ReifiedTestCase key blk
rehydrateTestCase = error "rehydrateTestCase: not implemented"
