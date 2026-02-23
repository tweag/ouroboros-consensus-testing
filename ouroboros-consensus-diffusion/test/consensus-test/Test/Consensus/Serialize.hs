{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Consensus.Serialize (
    AnchoredFork (..)
  , BlockRep (..)
  , FormatVersion (..)
  , ReifiedBlockTree (..)
  , ReifiedTestCase (..)
  , Seed (..)
  , TestVersion (..)
  , WithSlotNo (..)
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
import           Control.Monad.State (MonadState (..), State, runState)
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Functor ((<&>))
import qualified Data.Map as M
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

  , rtcPointSchedule :: PointSchedule (SlotNo, AF.BlockNo)

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
    FormatVersion _ -> do
      -- Currently we only have one format version.
      rtcTestKey <- obj .: "key"
      rtcTestVersion <- obj .: "testVersion"
      rtcBlockTree <- obj .: "blockTree"
      rtcPointSchedule <- obj .: "pointSchedule"
      rtcShrinkIndex <- obj .: "shrinkIndex"
      rtcSeed <- obj .: "seed"
      pure ReifiedTestCase {..}

instance (Aeson.ToJSON key) => Aeson.ToJSON (ReifiedTestCase key BlockRep) where
  toJSON = serializeReifiedTestCase (FormatVersion 0)

instance (Aeson.FromJSON key) => Aeson.FromJSON (ReifiedTestCase key BlockRep) where
  parseJSON = deserializeReifiedTestCase

-- | Construct a 'ReifiedTestCase' from a concrete test case.
toReifiedTestCase
  :: (AF.HasHeader blk)
  => key -> TestVersion -> BlockTree blk -> PointSchedule blk -> ShrinkIndex -> Seed
  -> ReifiedTestCase key BlockRep
toReifiedTestCase key testVersion blockTree pointSchedule shrinkIndex seed =
  ReifiedTestCase
    { rtcTestKey = key
    , rtcTestVersion = testVersion
    , rtcBlockTree = toReifiedBlockTree blockTree
    , rtcPointSchedule = toReifiedPointSchedule pointSchedule
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



-- How It Works --
------------------

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



-- Representation of Blocks --
------------------------------

-- | Representation of a block within a block tree. Since consensus tests do
-- not care about the contents of blocks, we only need enough information to
-- reconstruct the block tree using the methods in `IssueTestBlock` (and thus
-- do not otherwise care about the specific block type).
data BlockRep = BlockRep
  { brSlotGap :: SlotGap -- ^ Number of slots lapsed since the previous issued block.
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
  parseJSON = Aeson.withScientific "SlotGap" $ \n ->
    pure $ SlotGap (floor n :: Word64)

-- | Blocks in the block tree carry their slot number, but to issue
-- new blocks we need to know the slot /gap/ compared to the most
-- recently issued block. @WithSlotNo@ is a state monad that stores
-- the most recently used slot number so we can compute gaps.
newtype WithSlotNo a = WithSlotNo
  { unWithSlotNo :: State SlotNo a }
  deriving stock (Functor)
  deriving (Applicative, Monad, MonadState SlotNo) via (State SlotNo)

runWithSlotNo :: WithSlotNo a -> SlotNo -> (a, SlotNo)
runWithSlotNo action slotNo0 = runState (unWithSlotNo action) slotNo0

-- | Summarize a block as a @BlockRep@. @WithSlotNo@ is keeping track
-- of the most recently used slot number so we can compute the slot gap.
getBlockRep
  :: forall blk. (AF.HasHeader blk)
  => blk -> WithSlotNo (BlockRep, blk)
getBlockRep blk = do
  SlotNo lastSlotNo <- get
  let headers = AF.getHeaderFields blk
      SlotNo currentSlotNo = AF.headerFieldSlot headers
      slotGap = SlotGap (currentSlotNo - lastSlotNo)
      blockNo = AF.headerFieldBlockNo headers
  put (SlotNo currentSlotNo)
  pure (BlockRep slotGap blockNo, blk)

getBlockReps
  :: forall blk t. (AF.HasHeader blk, Traversable t)
  => t blk -> WithSlotNo (t (BlockRep, blk))
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
-- INVARIANT: the blocks in 'alBlocks' must be in order from oldest to newest,
-- and each block must be a valid successor of the previous block.
data AnchoredFork u = AnchoredFork
  { forkAnchor :: Maybe (SlotNo, u)
  , forkBlocks :: [u] -- Oldest first!
  , forkNumber :: Int
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Aeson.ToJSON u => Aeson.ToJSON (AnchoredFork u) where
  toJSON AnchoredFork{forkAnchor, forkBlocks, forkNumber} = Aeson.object
    [ "anchor" .= case forkAnchor of
        Nothing -> Aeson.String "genesis"
        Just (slotNo, rep) -> Aeson.object
          [ "slotNo" .= slotNo
          , "blockRep" .= rep
          ]
    , "blocks" .= forkBlocks
    , "forkNo" .= forkNumber
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
    forkNumber <- v .: "forkNo"
    pure AnchoredFork {..}



-- Conversion between ReifiedBlockTree and BlockTree --
-------------------------------------------------------

-- | Convert a @BlockTree@ to a @ReifiedBlockTree@. This direction
-- turns slot numbers into slot gaps.
toReifiedBlockTree
  :: forall blk. (AF.HasHeader blk)
  => BlockTree blk -> ReifiedBlockTree BlockRep
toReifiedBlockTree (BlockTree trunk branches) = ReifiedBlockTree
  (anchoredFragmentToAnchoredForkOldestFirst (trunk, 0))
  (fmap anchoredFragmentToAnchoredForkOldestFirst $ zip (fmap btbSuffix branches) [1..])
  where
    -- Represent an @AnchoredFragment@ as a list of @BlockRep@s, from oldest to
    -- newest, plus the anchor. @BlockTreeBranch@es include a lot of redundant information;
    -- all we need is the suffix of the branch (the part that is not shared with the trunk).
    anchoredFragmentToAnchoredForkOldestFirst
      :: (AF.AnchoredFragment blk, Int) -> AnchoredFork BlockRep
    anchoredFragmentToAnchoredForkOldestFirst (fragment, forkNo) =
      let
        anchor = getAnchorRep fragment
        slotNo = case anchor of
          Nothing           -> SlotNo 0
          Just (slotNum, _) -> slotNum
        -- The anchor's slot number is the one most recently used;
        -- we use that to compute slot gaps for the fragment.
        (fragment', _) = runWithSlotNo
          (getBlockReps (AF.toOldestFirst fragment)) slotNo
      in AnchoredFork anchor (fmap fst fragment') forkNo

    -- Summarize an anchored fragment's anchor.
    getAnchorRep
      :: AF.AnchoredFragment blk -> Maybe (SlotNo, BlockRep)
    getAnchorRep fragment = case AF.anchor fragment of
      AF.AnchorGenesis         -> Nothing
      AF.Anchor slot _ blockNo -> Just (slot, BlockRep (SlotGap $ unSlotNo slot) blockNo)

-- | Blocks in a block tree are uniquely identified by thier
-- slot and block numbers. @KnownBlocks@ maps these identifiers
-- to actual blocks.
type KnownBlocks blk = M.Map (SlotNo, AF.BlockNo) blk

-- | Given a @ReifiedBlockTree@, attempt to reconstruct the original @BlockTree@
-- by issuing blocks in order. (Trunk first, then branches one by one.) To do this
-- we keep track of previously issued blocks in a @KnownBlocks@ map. Morally there
-- is a little state monad going on here, but we're just passing the state manually
-- to keep it simple.
fromReifiedBlockTree
  :: forall blk. (AF.HasHeader blk, IssueTestBlock blk, Show blk)
  => ReifiedBlockTree BlockRep
  -> Either String (BlockTree blk, KnownBlocks blk)
fromReifiedBlockTree ReifiedBlockTree{rbtTrunk, rbtBranches} = do
  let
    -- Convert the anchor. If it is not genesis, we look up the
    -- block in a map of previously issued blocks.
    makeAnchor
      :: Maybe (SlotNo, BlockRep) -> KnownBlocks blk -> Either String (AF.Anchor blk)
    makeAnchor mAnchorRep knownBlocks = case mAnchorRep of
      Nothing -> Right AF.AnchorGenesis
      Just (slotNo, rep) -> case M.lookup (slotNo, brBlockNo rep) knownBlocks of
        Just blk ->
          let hash = AF.headerFieldHash (AF.getHeaderFields blk)
          in Right $ AF.Anchor slotNo hash (brBlockNo rep)
        Nothing   -> Left $
          "Failed to find anchor block for slot/blockNo: "
          <> show (slotNo, brBlockNo rep)
          <> " (from BlockRep " <> show rep <> ")"

    -- Issue the next block.
    issueNextBlock
      :: Int -- ^ Current fork number, needed to issue the first block on a branch
      -> Maybe blk -- ^ Anchor block, if this fork is anchored to a block
      -> ([blk], KnownBlocks blk, SlotNo) -- ^ Blocks issued so far (newest first) and known blocks
      -> BlockRep -- ^ Block to be issued
      -> Either String ([blk], KnownBlocks blk, SlotNo)
    issueNextBlock forkNo mAnchorBlk (accBlocks, knownBlocks, lastSlotNo) rep = do
      let slotDelta = SlotNo $ unSlotGap (brSlotGap rep)
      let lapsedSlots = case slotDelta of
            SlotNo 0 -> SlotNo 0
            SlotNo n -> SlotNo (n - 1)
      let currentSlotNo = lastSlotNo + fromIntegral (brSlotGap rep)
      blk <- Right $ case accBlocks of
        []  -> case mAnchorBlk of
          Nothing        -> issueFirstBlock forkNo slotDelta
          Just anchorBlk -> issueSuccessorBlock (Just forkNo) lapsedSlots anchorBlk
        h:_ -> issueSuccessorBlock Nothing lapsedSlots h
      pure
        ( blk : accBlocks
        , M.insert (currentSlotNo, brBlockNo rep) blk knownBlocks
        , currentSlotNo
        )

    -- Issue a chain of blocks.
    issueBlocks
      :: SlotNo -- ^ Last used slot number.
      -> Int -- ^ Current fork number
      -> Maybe blk -- ^ Anchor block, if any
      -- | Blocks to be issued, oldest first, and the known blocks so far.
      -> ([BlockRep], KnownBlocks blk)
      -- | Issued blocks, oldest first, and an updated map of blocks.
      -> Either String ([blk], KnownBlocks blk)
    issueBlocks lastSlotNo forkNo mAnchorBlk (reps, knownBlocks) = do
      (blocks, blocksById, _) <-
        foldM (issueNextBlock forkNo mAnchorBlk) ([], knownBlocks, lastSlotNo) reps
      pure (reverse blocks, blocksById)

    -- Issue a single fork (blocks plus anchor).
    issueFork
      -- | The fork to process and the known blocks so far.
      :: (AnchoredFork BlockRep, KnownBlocks blk)
      -> Either String (AF.AnchoredFragment blk, KnownBlocks blk)
    issueFork (AnchoredFork {forkAnchor, forkBlocks, forkNumber}, knownBlocks) = do
      anchor <- makeAnchor forkAnchor knownBlocks
      anchorBlock <- case forkAnchor of
        Nothing -> Right Nothing
        Just (slotNo, rep) ->
          case M.lookup (slotNo, brBlockNo rep) knownBlocks of
            Just blk -> Right (Just blk)
            Nothing  -> Left $
              "Failed to find anchor block payload for slot/blockNo: "
              <> show (slotNo, brBlockNo rep)
              <> " (from BlockRep " <> show rep <> ")"
      let lastSlotNo = case forkAnchor of
            Nothing          -> SlotNo 0
            Just (slotNo, _) -> slotNo
      (issuedBlocks, knownBlocks') <-
        issueBlocks lastSlotNo forkNumber anchorBlock (forkBlocks, knownBlocks)
      let fragment = AF.fromOldestFirst anchor issuedBlocks
      pure (fragment, knownBlocks')

    -- Issue the next fragment.
    issueNextFragment
      :: ([AF.AnchoredFragment blk], KnownBlocks blk)
      -> AnchoredFork BlockRep
      -> Either String ([AF.AnchoredFragment blk], KnownBlocks blk)
    issueNextFragment (fragments, knownBlocks) fork = do
      (fragment, knownBlocks') <- issueFork (fork, knownBlocks)
      pure (fragment : fragments, knownBlocks')

  (trunk, trunkBlocks) <- issueFork (rbtTrunk, M.empty)
  (branches, allBlocks) <- foldM issueNextFragment ([], trunkBlocks) (reverse rbtBranches)

  -- @fromTrunkAndBranches@ is a smart constructor for @BlockTree@ that ensures
  -- all the invariants are satisfied.
  case fromTrunkAndBranches trunk branches of
    Nothing -> Left $ mconcat
      [ "Failed to decode block tree! Some branches do not intersect the trunk.\n"
      , "Known block ids:\n", show (M.keys allBlocks)
      , "Trunk:\n", show trunk
      , "Branches:\n", show branches
      ]
    Just bt -> Right (bt, allBlocks)



-- Point Schedule Conversion --
-------------------------------

-- | Replace the blocks in a point schedule with corresponding
-- @SlotNo@s and @BlockNo@s.
toReifiedPointSchedule
  :: forall blk. (AF.HasHeader blk)
  => PointSchedule blk -> PointSchedule (SlotNo, AF.BlockNo)
toReifiedPointSchedule pointSchedule = pointSchedule <&> \blk ->
  let headers = AF.getHeaderFields blk
  in (AF.headerFieldSlot headers, AF.headerFieldBlockNo headers)

-- | During reconstruction of the block tree, we generated a map from block
-- identifiers (slot and block number) to actual blocks. We can use this with
-- @traverse@ to repopulate the point schedule.
fromReifiedPointSchedule
  :: M.Map (SlotNo, AF.BlockNo) blk -> PointSchedule (SlotNo, AF.BlockNo)
  -> Either String (PointSchedule blk)
fromReifiedPointSchedule blockTree schedule =
  let
    lookupBlockRep rep =
      case M.lookup rep blockTree of
        Just blk -> Right blk
        Nothing  -> Left $ "Failed to find block and slot number: " <> show rep
  in traverse lookupBlockRep schedule



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
data FormatVersion = FormatVersion Int
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON FormatVersion where
  toJSON (FormatVersion v) = Aeson.String (T.pack $ show v)

instance Aeson.FromJSON FormatVersion where
  parseJSON = Aeson.withText "FormatVersion" $ \txt ->
    case readMaybe (T.unpack txt) of
      Just v  -> pure (FormatVersion v)
      Nothing -> fail $ "Invalid FormatVersion: " <> T.unpack txt

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
