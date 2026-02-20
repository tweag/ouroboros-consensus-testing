{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
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
    BlockRep (..)
  , FormatVersion (..)
  , ReifiedBlockTree (..)
  , ReifiedTestCase (..)
  , Seed (..)
  , TestVersion (..)
  , deserializeReifiedTestCase
  , fromReifiedBlockTree
  , fromReifiedTestCase
  , getBlockRep
  , serializeReifiedTestCase
  , toReifiedBlockTree
  , toReifiedTestCase
  ) where

import           Cardano.Slotting.Slot (SlotNo (..))
import           Control.Monad (foldM)
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Foldable (toList)
import qualified Data.Map as M
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T
import           Data.Word (Word64)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.Block as AF
import           Test.Consensus.BlockTree
import           Test.Consensus.Genesis.Setup.GenChains (IssueTestBlock (..))
import           Test.Consensus.Genesis.ShrinkIndex
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
  { rtcTestKey       :: key
  -- ^ A key used by the test runner to identify a test or group of tests.

  , rtcTestVersion   :: TestVersion
  -- ^ Since serialized tests can exist beyond a single run, and tests can
  -- change over time, we need a way for the test case to specify which version
  -- of the test it was generated for.

  , rtcBlockTree     :: ReifiedBlockTree u
  -- ^ The block tree is represented as a trunk and a list of branches,
  -- oldest nodes first.

  , rtcPointSchedule :: PointSchedule u

  -- TODO: When this module moves to cardano-node, use ShrinkIndex here.
  , rtcShrinkIndex   :: ShrinkIndex
  -- ^ Used for specifying a shrink of the generated test case.

  , rtcSeed          :: Seed
  -- ^ Used for replaying tests.
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
data FormatVersion
  = FormatVersion_0_0
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON FormatVersion where
  toJSON FormatVersion_0_0 = Aeson.String "0.0"

instance Aeson.FromJSON FormatVersion where
  parseJSON = Aeson.withText "FormatVersion" $ \txt -> case txt of
    "0.0" -> pure FormatVersion_0_0
    _     -> fail $ "Unknown format version: " <> T.unpack txt

-- | A version number for the property test itself (as represented by 'key').
-- This is included to allow for backward compatibility in case the property
-- needs to change.
newtype TestVersion = TestVersion Int
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON TestVersion where
  toJSON (TestVersion v) = Aeson.String (T.pack $ show v)

instance Aeson.FromJSON TestVersion where
  parseJSON = Aeson.withText "TestVersion" $ \txt ->
    case readMaybe (T.unpack txt) of
      Just v  -> pure (TestVersion v)
      Nothing -> fail $ "Invalid TestVersion: " <> T.unpack txt

instance QC.Arbitrary TestVersion where
  arbitrary = do
    QC.NonNegative m <- QC.arbitrary
    pure $ TestVersion m
  shrink (TestVersion x) = do
    QC.NonNegative m <- QC.shrink (QC.NonNegative x)
    pure $ TestVersion m

-- | Construct a 'ReifiedTestCase' from a concrete test case.
toReifiedTestCase
  :: (AF.HasHeader blk, IssueTestBlock blk)
  => key -> TestVersion -> BlockTree blk -> PointSchedule blk -> ShrinkIndex -> Seed
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
  => (key -> TestVersion -> BlockTree blk -> PointSchedule blk -> ShrinkIndex -> Seed -> u)
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
  { rbtTrunk    :: AnchoredFork blk
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

type KnownHashes blk = M.Map BlockRep (AF.HeaderHash blk)

fromReifiedBlockTree
  :: forall blk. (AF.HasHeader blk, IssueTestBlock blk)
  => Proxy blk -> ReifiedBlockTree BlockRep -> Either String (BlockTree blk)
fromReifiedBlockTree _ ReifiedBlockTree{rbtTrunk, rbtBranches} = do
  let
    -- Convert the anchor. If it is not genesis, we look up the
    -- block hash in a map of previously issued block hashes.
    makeAnchor
      :: Maybe BlockRep -> KnownHashes blk -> Either String (AF.Anchor blk)
    makeAnchor mAnchorRep knownHashes = case mAnchorRep of
      Nothing -> Right AF.AnchorGenesis
      Just rep -> case M.lookup rep knownHashes of
        Just hash -> Right $ AF.Anchor (brSlotNo rep) hash (brBlockNo rep)
        Nothing   -> Left $ "Failed to find anchor block for BlockRep: " <> show rep

    -- Issue the next block.
    issueNextBlock
      :: Int -- ^ Current fork number
      -> ([blk], KnownHashes blk) -- ^ Blocks issued so far, plus known hashes
      -> BlockRep -- ^ Block to be issued
      -> Either String ([blk], KnownHashes blk)
    issueNextBlock forkNo (accBlocks, accHashes) rep = do
      blk <- Right $ case accBlocks of
        []  -> issueFirstBlock forkNo (brSlotNo rep)
        h:_ -> issueSuccessorBlock Nothing (brSlotNo rep) h
      let blkHash = AF.headerFieldHash (AF.getHeaderFields blk)
      pure (blk : accBlocks, M.insert rep blkHash accHashes)

    -- Issue a chain of blocks.
    issueBlocks
      :: Int -- ^ Current fork number
      -- | Blocks to be issued, oldest first, and the known hashes so far.
      -> ([BlockRep], KnownHashes blk)
      -- | Issued blocks, oldest first, and an updated map of hashes.
      -> Either String ([blk], KnownHashes blk)
    issueBlocks forkNo (reps, knownHashes) = do
      (blocks, hashes) <- foldM (issueNextBlock forkNo) ([], knownHashes) reps
      pure (reverse blocks, hashes)

    -- Issue a single fork (blocks plus anchor).
    issueFork
      -- | The fork to process, and the known hashes so far.
      :: (AnchoredFork BlockRep, KnownHashes blk)
      -> Either String (AF.AnchoredFragment blk, KnownHashes blk)
    issueFork (AnchoredFork {alAnchor, alBlocks, alForkNo}, knownHashes) = do
      anchor <- makeAnchor alAnchor knownHashes
      (issuedBlocks, knownHashes') <- issueBlocks alForkNo (alBlocks, knownHashes)
      let fragment = AF.fromOldestFirst anchor issuedBlocks
      pure (fragment, knownHashes')

    -- Issue the next fragment.
    issueNextFragment
      :: ([AF.AnchoredFragment blk], KnownHashes blk)
      -> AnchoredFork BlockRep
      -> Either String ([AF.AnchoredFragment blk], KnownHashes blk)
    issueNextFragment (fragments, knownHashes) fork = do
      (fragment, knownHashes') <- issueFork (fork, knownHashes)
      pure (fragment : fragments, knownHashes')

  (trunk, trunkHashes) <- issueFork (rbtTrunk, M.empty)
  (branches, _) <- foldM issueNextFragment ([], trunkHashes) rbtBranches

  case fromTrunkAndBranches trunk branches of
    Just bt -> Right bt
    Nothing -> Left "Failed to decode block tree"

-- | Representation of a block within a block tree. Since consensus tests do
-- not care about the contents of blocks, we only need enough information to
-- reconstruct the block tree using the methods in `IssueTestBlock` (and thus
-- do not otherwise care about the specific block type).
data BlockRep = BlockRep
  { brSlotNo  :: SlotNo
  , brBlockNo :: AF.BlockNo
  } deriving (Eq, Ord, Show)

instance (Aeson.ToJSON BlockRep) where
  toJSON BlockRep{ brSlotNo, brBlockNo } = Aeson.object
    [ "slotNo" .= brSlotNo
    , "blockNo" .= brBlockNo
    ]

instance (Aeson.FromJSON BlockRep) where
  parseJSON = Aeson.withObject "BlockRep" $ \v -> do
    brSlotNo <- fmap SlotNo $ v .: "slotNo"
    brBlockNo <- fmap AF.BlockNo $ v .: "blockNo"
    pure BlockRep {..}

-- | Summarize a block as a 'BlockRep'. Summarizable block types must
-- implement 'encodeHeaderHash' from 'IssueTestBlock'.
getBlockRep
  :: forall blk. (AF.HasHeader blk, IssueTestBlock blk)
  => blk -> BlockRep
getBlockRep blk =
  let headers = AF.getHeaderFields blk
  in BlockRep (AF.headerFieldSlot headers)
      (AF.headerFieldBlockNo headers)

-- | Get the summary of an anchored fragment's anchor.
getAnchorRep
  :: forall blk. (IssueTestBlock blk)
  => AF.AnchoredFragment blk -> Maybe BlockRep
getAnchorRep fragment = case AF.anchor fragment of
  AF.AnchorGenesis            -> Nothing
  AF.Anchor slot hash blockNo -> Just $ BlockRep slot blockNo

-- | @AnchoredFork@ is a simplified representation of an @AnchoredFragment@
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
        Nothing  -> Aeson.String "genesis"
        Just rep -> Aeson.toJSON rep
    , "blocks" .= alBlocks
    , "forkNo" .= show alForkNo
    ]

instance Aeson.FromJSON u => Aeson.FromJSON (AnchoredFork u) where
  parseJSON = Aeson.withObject "AnchoredFork" $ \v -> do
    alAnchor <- do
      val <- v .: "anchor"
      case val of
        Aeson.String "genesis" -> pure Nothing
        _                      -> Just <$> Aeson.parseJSON val
    alBlocks <- v .: "blocks"
    alForkNo <- do
      forkNoStr <- v .: "forkNo"
      case readMaybe forkNoStr of
        Just n  -> pure n
        Nothing -> fail $ "Invalid integer: " <> forkNoStr
    pure AnchoredFork {..}

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
        Nothing  -> Left $ "Failed to find block for BlockRep: " <> show rep
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
  , "seed" .= rtcSeed testCase
  ]

deserializeReifiedTestCase
  :: (Aeson.FromJSON key)
  => Aeson.Value -> Aeson.Parser (ReifiedTestCase key BlockRep)
deserializeReifiedTestCase = Aeson.withObject "ReifiedTestCase" $ \obj -> do
  fmtVersion <- obj .: "formatVersion"
  case fmtVersion of
    FormatVersion_0_0 -> do
      -- Currently we only have one format version.
      rtcTestKey <- obj .: "key"
      rtcTestVersion <- obj .: "testVersion"
      rtcBlockTree <- obj .: "blockTree"
      rtcPointSchedule <- obj .: "pointSchedule"
      rtcShrinkIndex <- obj .: "shrinkIndex"
      rtcSeed <- obj .: "seed"
      pure ReifiedTestCase {..}

instance (Aeson.ToJSON key) => Aeson.ToJSON (ReifiedTestCase key BlockRep) where
  toJSON = serializeReifiedTestCase FormatVersion_0_0

instance (Aeson.FromJSON key) => Aeson.FromJSON (ReifiedTestCase key BlockRep) where
  parseJSON = deserializeReifiedTestCase
