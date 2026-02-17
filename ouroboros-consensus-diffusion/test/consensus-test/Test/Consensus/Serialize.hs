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
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Consensus.Serialize (
    ReifiedTestCase(..)
  , toReifiedTestCase
  , fromReifiedTestCase
  , serializeReifiedTestCase
  , deserializeReifiedTestCase
  , ReifiedBlockTree(..)
  , toReifiedBlockTree
  , fromReifiedBlockTree
  , BlockRep(..)
  , getBlockRep
  , TestVersion(..)
  , FormatVersion(..)
) where

import           Cardano.Slotting.Slot (SlotNo(..))
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
import           Data.Foldable (toList)
import qualified Data.Map as M
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.Block as AF
import           Test.Consensus.BlockTree
import           Test.Consensus.Genesis.Setup.GenChains (IssueTestBlock(..))
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random
import           Text.Read

-- This module implements JSON serialization for consensus test cases. The
-- main interface comprises:
--
--   - 'ReifiedTestCase': a parameterized representation of a single
--     test case.
--   - 'serializeReifiedTestCase' and 'deserializeReifiedTestCase': functions
--      for converting between 'ReifiedTestCase' and 'Aeson.Value'.
--   - 'fromReifiedTestCase' and 'toReifiedTestCase': functions for converting
--      between a 'ReifiedTestCase' and a concrete test case type of your choice.
--
-- The JSON format is meant to be as stable as possible under changes to
-- test case generation. This is achieved via a test version number and
-- a serialization format version number, both of which are included in the JSON.

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

instance (Eq key, Eq u) => Eq (ReifiedTestCase key u) where
  a == b =
    rtcTestKey a == rtcTestKey b &&
    rtcTestVersion a == rtcTestVersion b &&
    rtcBlockTree a == rtcBlockTree b &&
    rtcPointSchedule a == rtcPointSchedule b &&
    rtcShrinkIndex a == rtcShrinkIndex b &&
    show (rtcSeed a) == show (rtcSeed b)
    -- QCGen does not have an Eq instance, but
    -- Read/Show is a canonical serialization.

-- | A version number for the serialization format. This is included to
-- allow for backward compatibility in case the JSON format needs to change.
-- This only exists in the JSON, and consumers of this library should not
-- use or rely on it.
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

-- | Construct a 'ReifiedTestCase' from a concrete test case.
toReifiedTestCase
  :: (AF.HasHeader blk, IssueTestBlock blk)
  => key -> TestVersion -> BlockTree blk -> PointSchedule blk -> [Int] -> QCGen
  -> ReifiedTestCase key BlockRep
toReifiedTestCase key testVersion blockTree pointSchedule shrinkIndex seed =
  ReifiedTestCase
    { rtcTestKey = key
    , rtcTestVersion = testVersion
    , rtcBlockTree = toReifiedBlockTree blockTree
    , rtcPointSchedule = fmap getBlockRep pointSchedule
    , rtcShrinkIndex = shrinkIndex
    , rtcSeed = seed
    }

-- | Deconstruct a 'ReifiedTestCase' into a type of your choice using a continuation.
fromReifiedTestCase
  :: forall blk key u. (AF.HasHeader blk, IssueTestBlock blk)
  => (key -> TestVersion -> BlockTree blk -> PointSchedule blk -> [Int] -> QCGen -> u)
  -> ReifiedTestCase key BlockRep -> Either String u
fromReifiedTestCase f ReifiedTestCase{..} = do
  blockTree <- fromReifiedBlockTree (Proxy :: Proxy blk) rtcBlockTree
  pointSchedule <- fromReifiedPointSchedule blockTree rtcPointSchedule
  pure $ f rtcTestKey rtcTestVersion blockTree pointSchedule rtcShrinkIndex rtcSeed

-- | Representation of the trunk and branches of a block tree as lists, oldest
-- nodes first. Meant to be as normalized as possible and efficient to convert
-- in both directions. Branches are represented as suffixes off the trunk, where
-- the anchor is a trunk node; this is enough to reconstruct the tree without
-- redundant information.
data ReifiedBlockTree blk = ReifiedBlockTree
  { rbtTrunk :: AnchoredFork blk
  , rbtBranches :: [AnchoredFork blk]
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

-- | Summarize a block tree as a 'ReifiedBlockTree'. This is the representation
-- we will serialize.
toReifiedBlockTree
  :: forall blk. (AF.HasHeader blk, IssueTestBlock blk)
  => BlockTree blk -> ReifiedBlockTree BlockRep
toReifiedBlockTree (BlockTree trunk branches) = ReifiedBlockTree
  (anchoredFragmentToAnchoredForkOldestFirst 0 trunk)
  (fmap (uncurry buildReifiedBranch) (zip branches [1..]))
  where
    buildReifiedBranch :: BlockTreeBranch blk -> Int -> AnchoredFork BlockRep
    buildReifiedBranch branch forkNo =
      anchoredFragmentToAnchoredForkOldestFirst forkNo (btbSuffix branch)

    -- | Represent an 'AnchoredFragment' as a list of 'BlockRep's, from oldest to
    -- newest, plus the anchor.
    anchoredFragmentToAnchoredForkOldestFirst
      :: Int -> AF.AnchoredFragment blk -> AnchoredFork BlockRep
    anchoredFragmentToAnchoredForkOldestFirst forkNo fragment = AnchoredFork
      (getAnchorRep fragment) (fmap getBlockRep (AF.toOldestFirst fragment)) forkNo

fromReifiedBlockTree
  :: (AF.HasHeader blk, IssueTestBlock blk)
  => Proxy blk -> ReifiedBlockTree BlockRep -> Either String (BlockTree blk)
fromReifiedBlockTree _ ReifiedBlockTree{rbtTrunk, rbtBranches} = do
  let trunk = anchoredForkToAnchoredFragment rbtTrunk
      branches = fmap anchoredForkToAnchoredFragment rbtBranches
  case fromTrunkAndBranches trunk branches of
    Just bt -> Right bt
    Nothing -> Left "Failed to decode block tree"

-- | Representation of a block within a block tree. Since consensus tests do
-- not care about the contents of blocks, we only need enough information to
-- reconstruct the block tree using the methods in `IssueTestBlock` (and thus
-- do not otherwise care about the specific block type).
data BlockRep = BlockRep
  { brSlotNo  :: SlotNo
  , brHash    :: T.Text
  , brBlockNo :: AF.BlockNo
  } deriving (Eq, Ord, Show)

instance (Aeson.ToJSON BlockRep) where
  toJSON BlockRep{ brSlotNo, brHash, brBlockNo } = Aeson.object
    [ "slotNo" .= brSlotNo
    , "hash" .= brHash
    , "blockNo" .= brBlockNo
    ]

instance (Aeson.FromJSON BlockRep) where
  parseJSON = Aeson.withObject "BlockRep" $ \v -> do
    brSlotNo <- v .: "slotNo"
    brHash <- v .: "hash"
    brBlockNo <- v .: "blockNo"
    pure BlockRep {..}

-- | Summarize a block as a 'BlockRep'. Summarizable block types must
-- implement 'encodeHeaderHash' from 'IssueTestBlock'.
getBlockRep
  :: forall blk. (AF.HasHeader blk, IssueTestBlock blk)
  => blk -> BlockRep
getBlockRep blk =
  let headers = AF.getHeaderFields blk
  in BlockRep (AF.headerFieldSlot headers)
      (encodeHeaderHash (Proxy :: Proxy blk) (AF.headerFieldHash headers))
      (AF.headerFieldBlockNo headers)

-- | Get the summary of an anchored fragment's anchor.
getAnchorRep
  :: forall blk. (IssueTestBlock blk)
  => AF.AnchoredFragment blk -> Maybe BlockRep
getAnchorRep fragment = case AF.anchor fragment of
  AF.AnchorGenesis -> Nothing
  AF.Anchor slot hash blockNo -> Just $ BlockRep slot
    (encodeHeaderHash (Proxy :: Proxy blk) hash) blockNo

-- | 'AnchoredFork' is a simplified representation of an 'AnchoredFragment'
-- as a list; an anchor of 'Nothing' represents the genesis.
--
-- INVARIANT: the blocks in 'alBlocks' must be in order from oldest to newest,
-- and each block must be a valid successor of the previous block.
data AnchoredFork u = AnchoredFork
  { alAnchor :: Maybe u
  , alBlocks :: [u] -- Oldest first!
  , alForkNo :: Int
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Aeson.ToJSON u => Aeson.ToJSON (AnchoredFork u) where
  toJSON AnchoredFork{alAnchor, alBlocks, alForkNo} = Aeson.object
    [ "anchor" .= case alAnchor of
        Nothing -> Aeson.String "genesis"
        Just rep -> Aeson.toJSON rep
    , "blocks" .= alBlocks
    , "forkNo" .= alForkNo
    ]

instance Aeson.FromJSON u => Aeson.FromJSON (AnchoredFork u) where
  parseJSON = Aeson.withObject "AnchoredFork" $ \v -> do
    alAnchor <- do
      val <- v .: "anchor"
      case val of
        Aeson.String "genesis" -> pure Nothing
        _ -> Just <$> Aeson.parseJSON val
    alBlocks <- v .: "blocks"
    alForkNo <- v .: "forkNo"
    pure AnchoredFork {..}

anchoredForkToAnchoredFragment
  :: forall blk. (AF.HasHeader blk, IssueTestBlock blk)
  => AnchoredFork BlockRep -> AF.AnchoredFragment blk
anchoredForkToAnchoredFragment fragment =
  let
    AnchoredFork{alAnchor, alBlocks, alForkNo} = fragment
    -- Convert the anchor:
    anchor = case alAnchor of
      Nothing -> AF.AnchorGenesis
      Just rep -> case decodeHeaderHash (Proxy :: Proxy blk) (brHash rep) of
        Right hash -> AF.Anchor (brSlotNo rep) hash (brBlockNo rep)
        Left err -> error err
    -- Issue blocks for the headers:
    convertBlockReps :: [BlockRep] -> [blk]
    convertBlockReps reps =
      let
        folder :: BlockRep -> [blk] -> [blk]
        folder rep acc = case acc of
          []  -> issueFirstBlock alForkNo (brSlotNo rep) : acc
          h:_ -> issueSuccessorBlock Nothing (brSlotNo rep) h : acc
      in foldr folder [] reps
  in AF.fromOldestFirst anchor $ reverse $ convertBlockReps alBlocks

fromReifiedPointSchedule
  :: forall blk. (AF.HasHeader blk, IssueTestBlock blk)
  => BlockTree blk -> PointSchedule BlockRep -> Either String (PointSchedule blk)
fromReifiedPointSchedule blockTree schedule =
  let
    blockRepMap :: M.Map BlockRep blk
    blockRepMap = M.fromList $ fmap (\blk -> (getBlockRep blk, blk)) (toList blockTree)

    lookupBlockRep :: BlockRep -> Either String blk
    lookupBlockRep rep =
      case M.lookup rep blockRepMap of
        Just blk -> Right blk
        Nothing -> Left $ "Failed to find block for BlockRep: " ++ show rep
  in traverse lookupBlockRep schedule

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
