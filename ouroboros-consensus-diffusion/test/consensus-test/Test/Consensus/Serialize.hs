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
    -- $howitworks
    AnchoredFork (..)
  , BlockId (..)
  , BlockRep (..)
  , ForkNo (..)
  , FormatVersion (..)
  , KnownBlocks (..)
  , KnownForks (..)
  , ReifiedBlockTree (..)
  , ReifiedTestCase (..)
  , Seed (..)
  , TestVersion (..)
  , deserializeReifiedTestCase
  , fromReifiedBlockTree
  , fromReifiedPointSchedule
  , fromReifiedTestCase
  , getBlockRep
  , getBlockReps
  , serializeReifiedTestCase
  , toReifiedBlockTree
  , toReifiedPointSchedule
  , toReifiedTestCase
  ) where

import           Cardano.Slotting.Slot (SlotNo (..))
import           Control.Monad (foldM)
import           Control.Monad.State (MonadState (..), runState)
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Functor ((<&>))
import qualified Data.Map as M
import           Data.Scientific (toBoundedInteger)
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
import           Text.Read (readMaybe)

-- $intro
-- This module implements JSON serialization for consensus test cases. The
-- interface comprises:
--
--   - 'ReifiedTestCase': a parameterized representation of a single
--     test case.
--   - 'serializeReifiedTestCase' and 'deserializeReifiedTestCase': functions
--     for converting between 'ReifiedTestCase' and 'Aeson.Value'.
--   - 'fromReifiedTestCase' and 'toReifiedTestCase': functions for converting
--     between a 'ReifiedTestCase' and a concrete test case type of your choice.
--
-- The JSON format is meant to be as stable as possible under changes to
-- test case generation. This is achieved via a test version number and
-- a serialization format version number, both of which are included in the JSON.

-- $howitworks
-- We could have added Generic instances to everything and derived ToJSON/FromJSON.
-- That is not ideal however; first because generated test cases contain a lot of
-- redundant information (a LOT). Second, because this would tie the serialization
-- format to the internal representation, making it difficult to change both. We
-- expect serialized test cases to persist beyond a single run, so the format needs
-- to be stable under refactorings.
--
-- This implementation instead uses custom JSON instances, a simplified model of the
-- block tree that only includes the trunk and branch suffixes, and a representation
-- of blocks that only includes the data necessary to issue them in order.

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

  , rtcPointSchedule :: PointSchedule BlockId

  , rtcShrinkIndex   :: ShrinkIndex
  -- ^ Used for specifying a shrink of the generated test case.

  , rtcSeed          :: Seed
  -- ^ Used for replaying tests.
  } deriving (Eq, Show, Functor, Foldable, Traversable)

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
    FormatVersionOne -> do
      -- Currently we only have one format version.
      rtcTestKey <- obj .: "key"
      rtcTestVersion <- obj .: "testVersion"
      rtcBlockTree <- obj .: "blockTree"
      rtcPointSchedule <- obj .: "pointSchedule"
      rtcShrinkIndex <- obj .: "shrinkIndex"
      rtcSeed <- obj .: "seed"
      pure ReifiedTestCase {..}

instance (Aeson.ToJSON key) => Aeson.ToJSON (ReifiedTestCase key BlockRep) where
  toJSON = serializeReifiedTestCase FormatVersionOne

instance (Aeson.FromJSON key) => Aeson.FromJSON (ReifiedTestCase key BlockRep) where
  parseJSON = deserializeReifiedTestCase

-- | Construct a 'ReifiedTestCase' from a concrete test case.
toReifiedTestCase
  :: (AF.HasHeader blk, Show blk)
  => key -> TestVersion -> BlockTree blk -> PointSchedule blk -> ShrinkIndex -> Seed
  -> ReifiedTestCase key BlockRep
toReifiedTestCase key testVersion blockTree pointSchedule shrinkIndex seed =
  let
    (reifiedBlockTree, knownForks) = toReifiedBlockTree blockTree
    reifiedPointSchedule = toReifiedPointSchedule knownForks pointSchedule
  in ReifiedTestCase
    { rtcTestKey = key
    , rtcTestVersion = testVersion
    , rtcBlockTree = reifiedBlockTree
    , rtcPointSchedule = reifiedPointSchedule
    , rtcShrinkIndex = shrinkIndex
    , rtcSeed = seed
    }

-- | Deconstruct a @ReifiedTestCase@ into a type of your choice using a continuation.
-- This decouples serialization from the specific test case type. If it helps you can
-- think of this as a fold.
fromReifiedTestCase
  :: forall blk key u. (AF.HasHeader blk, IssueTestBlock blk, Show blk)
  => (key -> TestVersion -> BlockTree blk -> PointSchedule blk -> ShrinkIndex -> Seed -> u)
  -> ReifiedTestCase key BlockRep -> Either String u
fromReifiedTestCase f ReifiedTestCase{..} = do
  (blockTree, knownBlocks) <- fromReifiedBlockTree rtcBlockTree
  pointSchedule <- fromReifiedPointSchedule knownBlocks rtcPointSchedule
  pure $ f rtcTestKey rtcTestVersion blockTree pointSchedule rtcShrinkIndex rtcSeed



-- Representation of Blocks --
------------------------------

-- | Representation of a block within a block tree. Since consensus tests do
-- not care about the contents of blocks, we only need enough information to
-- reconstruct the block tree using the methods in `IssueTestBlock` (and thus
-- do not otherwise care about the specific block type).
data BlockRep = BlockRep
  { brSlotGap :: SlotGap  -- ^ Number of slots lapsed since the previous issued block.
  , brBlockNo :: AF.BlockNo
  } deriving (Eq, Ord, Show)

instance (Aeson.ToJSON BlockRep) where
  toJSON BlockRep{ brSlotGap, brBlockNo } = Aeson.object
    [ "slotGap" .= brSlotGap
    , "blockNo" .= brBlockNo
    ]

instance (Aeson.FromJSON BlockRep) where
  parseJSON = Aeson.withObject "BlockRep" $ \v -> do
    brSlotGap <- v .: "slotGap"
    brBlockNo <- fmap AF.BlockNo $ v .: "blockNo"
    pure BlockRep {..}

-- | There are two numbers relevant to slots running around. @SlotNo@ is an
-- ordinal number that identifies a slot. @SlotGap@ is a cardinal number that
-- counts how many slots have expired since the last issued block; this is
-- what IssueTestBlock needs to issue the next block.
newtype SlotGap = SlotGap { unSlotGap :: Word64 }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Real, Enum, Integral)

instance Aeson.ToJSON SlotGap where
  toJSON (SlotGap gap) = Aeson.Number (fromIntegral gap)

instance Aeson.FromJSON SlotGap where
  parseJSON = Aeson.withScientific "SlotGap" $ \sci ->
    case toBoundedInteger sci of
      Just v  -> pure (SlotGap v)
      Nothing -> fail $ "Invalid SlotGap: " <> show sci

offsetSlotNo :: SlotNo -> SlotGap -> SlotNo
offsetSlotNo (SlotNo slot) (SlotGap gap) = SlotNo (slot + gap)

-- | Summarize a block as a @BlockRep@. @m@ is keeping track
-- of the most recently used slot number so we can compute the slot gap.
getBlockRep
  :: forall blk m. (AF.HasHeader blk, MonadState SlotNo m)
  => blk -> m (BlockRep, blk)
getBlockRep blk = do
  SlotNo lastSlotNo <- get
  let headers = AF.getHeaderFields blk
      SlotNo currentSlotNo = AF.headerFieldSlot headers
      slotGap = SlotGap (currentSlotNo - lastSlotNo)
      blockNo = AF.headerFieldBlockNo headers
  put (SlotNo currentSlotNo)
  pure (BlockRep slotGap blockNo, blk)

getBlockReps
  :: forall blk t m. (AF.HasHeader blk, Traversable t, MonadState SlotNo m)
  => t blk -> m (t (BlockRep, blk))
getBlockReps = traverse getBlockRep



-- Representation of Block Trees --
-----------------------------------

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

-- | @AnchoredFork@ is a simplified representation of an @AnchoredFragment@
-- as a list that also remembers its fork number. An anchor of 'Nothing'
-- represents the genesis.
--
-- INVARIANT: the blocks in 'forkBlocks' must be in order from oldest to newest,
-- and each block must be a valid successor of the previous block.
data AnchoredFork u = AnchoredFork
  { forkAnchor :: Maybe (SlotNo, u)
  , forkBlocks :: [u] -- ^ Oldest first!
  , forkNumber :: ForkNo
  } deriving (Eq, Show, Functor, Foldable, Traversable)

newtype ForkNo = ForkNo { unForkNo :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

instance Aeson.ToJSON u => Aeson.ToJSON (AnchoredFork u) where
  toJSON AnchoredFork{forkAnchor, forkBlocks, forkNumber} = Aeson.object
    [ "anchor" .= case forkAnchor of
        Nothing -> Aeson.String "genesis"
        Just (slotNo, rep) -> Aeson.object
          [ "slotNo" .= slotNo
          , "blockRep" .= rep
          ]
    , "blocks" .= forkBlocks
    , "forkNo" .= unForkNo forkNumber
    ]

instance Aeson.FromJSON u => Aeson.FromJSON (AnchoredFork u) where
  parseJSON = Aeson.withObject "AnchoredFork" $ \v -> do
    forkAnchor <- do
      val <- v .: "anchor"
      case val of
        Aeson.String s | s == "genesis" -> pure Nothing
        Aeson.Object obj' -> do
          slotNo <- obj' .: "slotNo"
          blockRep <- obj' .: "blockRep"
          pure $ Just (slotNo, blockRep)
        _ -> fail "Invalid anchor format"
    forkBlocks <- v .: "blocks"
    forkNumber <- fmap ForkNo $ v .: "forkNo"
    pure AnchoredFork {..}



-- Conversion between ReifiedBlockTree and BlockTree --
-------------------------------------------------------

-- | Convert a @BlockTree@ to a @ReifiedBlockTree@. This direction
-- turns slot numbers into slot gaps.
toReifiedBlockTree
  :: forall blk. (AF.HasHeader blk)
  => BlockTree blk -> (ReifiedBlockTree BlockRep, KnownForks blk)
toReifiedBlockTree (BlockTree trunk branches) =
  let
    (reifiedTrunk, knownForksTrunk) = anchoredFragmentToAnchoredForkOldestFirst (trunk, ForkNo 0)
    (reifiedBranches, knownForksBranches) = unzip $ fmap anchoredFragmentToAnchoredForkOldestFirst
      (zip (reverse $ fmap btbSuffix branches) (fmap ForkNo [1..]))
  in
    ( ReifiedBlockTree reifiedTrunk reifiedBranches
    , mconcat (knownForksTrunk : knownForksBranches)
    )
  where
    -- Represent an @AnchoredFragment@ as a list of @BlockRep@s, from oldest to
    -- newest, plus the anchor. @BlockTreeBranch@es include a lot of redundant information;
    -- all we need is the suffix of the branch (the part that is not shared with the trunk).
    anchoredFragmentToAnchoredForkOldestFirst
      :: (AF.AnchoredFragment blk, ForkNo) -> (AnchoredFork BlockRep, KnownForks blk)
    anchoredFragmentToAnchoredForkOldestFirst (fragment, forkNo) =
      let
        anchor = getAnchorRep fragment
        slotNo = case anchor of
          Nothing           -> SlotNo 0
          Just (slotNum, _) -> slotNum
        -- The anchor's slot number is the one most recently used;
        -- we use that to compute slot gaps for the fragment.
        (fragment', _) = runState
          (getBlockReps (AF.toOldestFirst fragment)) slotNo
        knownForks = KnownForks $ M.fromList
          [ (AF.headerFieldHash (AF.getHeaderFields blk), forkNo)
          | (_, blk) <- fragment'
          ]
      in (AnchoredFork anchor (fmap fst fragment') forkNo, knownForks)

    -- Summarize an anchored fragment's anchor.
    getAnchorRep
      :: AF.AnchoredFragment blk -> Maybe (SlotNo, BlockRep)
    getAnchorRep fragment = case AF.anchor fragment of
      AF.AnchorGenesis         -> Nothing
      AF.Anchor slot _ blockNo -> Just (slot, BlockRep (SlotGap $ unSlotNo slot) blockNo)

-- | Blocks in a block tree are uniquely identified by their fork,
-- slot, and block numbers. @KnownBlocks@ maps these identifiers
-- to actual blocks.
newtype KnownBlocks blk = KnownBlocks { unKnownBlocks :: M.Map BlockId blk }
  deriving newtype (Semigroup, Monoid)



newtype KnownForks blk = KnownForks { unKnownForks :: M.Map (AF.HeaderHash blk) ForkNo }

deriving instance (Ord (AF.HeaderHash blk)) => Semigroup (KnownForks blk)
deriving instance (Ord (AF.HeaderHash blk)) => Monoid (KnownForks blk)

data BlockId = BlockId
  { bidSlotNo  :: SlotNo
  , bidBlockNo :: AF.BlockNo
  , bidForkNo  :: ForkNo
  } deriving (Eq, Ord, Show)

instance Aeson.ToJSON BlockId where
  toJSON BlockId { bidSlotNo, bidBlockNo, bidForkNo } = Aeson.object
    [ "slotNo" .= bidSlotNo
    , "blockNo" .= bidBlockNo
    , "forkNo" .= unForkNo bidForkNo
    ]

instance Aeson.FromJSON BlockId where
  parseJSON = Aeson.withObject "BlockId" $ \v -> do
    bidSlotNo <- v .: "slotNo"
    bidBlockNo <- fmap AF.BlockNo $ v .: "blockNo"
    bidForkNo <- fmap ForkNo $ v .: "forkNo"
    pure BlockId {..}

lookupKnownBlock :: BlockId -> KnownBlocks blk -> Maybe blk
lookupKnownBlock blockId (KnownBlocks m) = M.lookup blockId m

insertKnownBlock :: BlockId -> blk -> KnownBlocks blk -> KnownBlocks blk
insertKnownBlock blockId blk (KnownBlocks m) = KnownBlocks (M.insert blockId blk m)

knownBlockIds :: KnownBlocks blk -> [BlockId]
knownBlockIds (KnownBlocks m) = M.keys m

-- | Given a @ReifiedBlockTree@, attempt to reconstruct the original @BlockTree@
-- by issuing blocks in order. (Trunk first, then branches one by one.) To do this
-- we keep track of previously issued blocks in a @KnownBlocks@ map. Morally there
-- is a little state monad going on here, but we're just passing the state manually
-- to keep it simple. @KnownBlocks@ is needed for two reasons: to look up anchor
-- blocks when forking, and to rehydrate the point schedule.
--
-- This function is a nested fold:
--   * for each branch (including the trunk, which is special),
--     * for each block rep (including the anchor, which is special),
--       * issue the block and add it to the known block map
--     * then convert to an anchored fragment
--   * then try to assemble the converted trunk and branches to a block tree.
--
-- TODO: Unify this with the tree generation code in @genChains@.
fromReifiedBlockTree
  :: forall blk. (AF.HasHeader blk, IssueTestBlock blk, Show blk)
  => ReifiedBlockTree BlockRep
  -> Either String (BlockTree blk, KnownBlocks blk)
fromReifiedBlockTree ReifiedBlockTree{rbtTrunk, rbtBranches} = do
  let
    -- Construct the anchor of a chain. For non-trunk branches we
    -- need to make sure the anchor has already been issued; it's
    -- enough to convert the trunk first since all fork anchors
    -- are there. For the trunk the anchor is always genesis.
    makeAnchor
      :: Maybe BlockId -> KnownBlocks blk -> Either String (AF.Anchor blk)
    makeAnchor mAnchorId knownBlocks = case mAnchorId of
      Nothing -> Right AF.AnchorGenesis
      Just blockId -> case lookupKnownBlock blockId knownBlocks of
        Just blk ->
          let BlockId slotNo blockNo _ = blockId
              hash = AF.headerFieldHash (AF.getHeaderFields blk)
          in Right $ AF.Anchor slotNo hash blockNo
        Nothing   -> Left $
          "Failed to find anchor block for slot/blockNo: "
          <> show blockId

    issueNextBlockOnFork
      :: ForkNo  -- Current fork number, needed to issue the first block on a branch
      -> Maybe blk  -- Anchor block, if this fork is anchored to a block
      -> ([blk], KnownBlocks blk, SlotNo)  -- Blocks issued so far (newest first) and known blocks
      -> BlockRep  -- Block to be issued
      -> Either String ([blk], KnownBlocks blk, SlotNo)
    issueNextBlockOnFork forkNo mAnchorBlk (accBlocks, knownBlocks, lastSlotNo) rep = do
      let
        forkNo' = unForkNo forkNo
        slotGap = brSlotGap rep
        -- issueFirstBlock and issueSuccessorBlock treat their SlotNo
        -- arguments differently; successor treats it like an offset
        -- and adds one, and first treats it like an index.
        slotNumber = offsetSlotNo lastSlotNo slotGap -- absolute slot number
        slotSuccOffset = offsetSlotNo 0 (slotGap - 1) -- offset from last slot number minus one

      blk <- pure $ case accBlocks of
        []  -> case mAnchorBlk of
          Nothing        -> issueFirstBlock forkNo' slotNumber
          Just anchorBlk -> issueSuccessorBlock (Just forkNo') slotSuccOffset anchorBlk
        h:_ -> issueSuccessorBlock Nothing slotSuccOffset h

      let blockId = BlockId slotNumber (brBlockNo rep) forkNo
      pure
        ( blk : accBlocks
        , insertKnownBlock blockId blk knownBlocks
        , slotNumber
        )

    issueBlocks
      :: SlotNo  -- Last used slot number.
      -> ForkNo  -- Current fork number
      -> Maybe blk  -- Anchor block, if any
      -- Blocks to be issued, oldest first, and the known blocks so far.
      -> ([BlockRep], KnownBlocks blk)
      -- Issued blocks, oldest first, and an updated map of blocks.
      -> Either String ([blk], KnownBlocks blk)
    issueBlocks lastSlotNo forkNo mAnchorBlk (reps, knownBlocks) = do
      (blocks, blocksById, _) <-
        foldM (issueNextBlockOnFork forkNo mAnchorBlk) ([], knownBlocks, lastSlotNo) reps
      pure (reverse blocks, blocksById)

    -- Issue a single fork (blocks plus anchor, the trunk is also a fork).
    issueFork
      -- The fork to process and the known blocks so far.
      :: (AnchoredFork BlockRep, KnownBlocks blk)
      -> Either String (AF.AnchoredFragment blk, KnownBlocks blk)
    issueFork (AnchoredFork {forkAnchor, forkBlocks, forkNumber}, knownBlocks) = do
      let
        mAnchorBlockId :: Maybe BlockId
        mAnchorBlockId = forkAnchor <&> \(slotNo, rep) ->
          -- Anchor blocks are always on the trunk (aka fork zero)
          BlockId slotNo (brBlockNo rep) (ForkNo 0)
      anchor <- makeAnchor mAnchorBlockId knownBlocks
      anchorBlock <- case mAnchorBlockId of
        Nothing -> Right Nothing
        Just blockId -> case lookupKnownBlock blockId knownBlocks of
          Just blk -> Right (Just blk)
          Nothing  -> Left $
            "Failed to find anchor block for blockId: " <> show blockId
      let lastSlotNo = maybe (SlotNo 0) fst forkAnchor
      (issuedBlocks, knownBlocks') <-
        issueBlocks lastSlotNo forkNumber anchorBlock (forkBlocks, knownBlocks)
      let fragment = AF.fromOldestFirst anchor issuedBlocks
      pure (fragment, knownBlocks')

    issueNextBranch
      :: ([AF.AnchoredFragment blk], KnownBlocks blk)
      -> AnchoredFork BlockRep
      -> Either String ([AF.AnchoredFragment blk], KnownBlocks blk)
    issueNextBranch (fragments, knownBlocks) fork = do
      (fragment, knownBlocks') <- issueFork (fork, knownBlocks)
      pure (fragment : fragments, knownBlocks')

  -- Issue the trunk blocks first so that the fork anchors will be in the known blocks map
  (trunk, trunkBlocks) <- issueFork (rbtTrunk, mempty)
  (branches, allBlocks) <- foldM issueNextBranch ([], trunkBlocks) (reverse rbtBranches)

  -- @fromTrunkAndBranches@ is a smart constructor for @BlockTree@ that ensures
  -- all the invariants are satisfied.
  case fromTrunkAndBranches trunk branches of
    Nothing -> Left $ mconcat
      [ "Failed to decode block tree! Some branches do not intersect the trunk.\n"
      , "Known block ids:\n", show (knownBlockIds allBlocks)
      , "Trunk:\n", show trunk
      , "Branches:\n", show branches
      ]
    Just bt -> Right (bt, allBlocks)



-- Point Schedule Conversion --
-------------------------------

lookupKnownFork
  :: (Ord (AF.HeaderHash blk))
  => AF.HeaderHash blk -> KnownForks blk -> Maybe ForkNo
lookupKnownFork blockHash (KnownForks m) = M.lookup blockHash m

-- | Replace the blocks in a point schedule with corresponding @BlockId@s.
toReifiedPointSchedule
  :: forall blk. (AF.HasHeader blk, Show blk)
  => KnownForks blk -> PointSchedule blk -> PointSchedule BlockId
toReifiedPointSchedule knownForks pointSchedule = pointSchedule <&> \blk ->
  let
    headers = AF.getHeaderFields blk
    slotNo = AF.headerFieldSlot headers
    blockNo = AF.headerFieldBlockNo headers
    blockHash = AF.headerFieldHash headers
  in case lookupKnownFork blockHash knownForks of
      Just forkNo -> BlockId slotNo blockNo forkNo
      Nothing -> error $ mconcat
        [ "Failed to find fork number for block while reifying point schedule: "
        , show (slotNo, blockNo)
        , " block hash: "
        , show blockHash
        , " block: "
        , show blk
        ]

-- | During reconstruction of the block tree, we generated a map from block ids
-- to blocks. Use this with @traverse@ to repopulate the point schedule.
fromReifiedPointSchedule
  :: KnownBlocks blk -> PointSchedule BlockId
  -> Either String (PointSchedule blk)
fromReifiedPointSchedule blockTree = traverse $
  \blockId -> case lookupKnownBlock blockId blockTree of
    Just blk -> Right blk
    Nothing  -> Left $ "Failed to find block id: " <> show blockId



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
